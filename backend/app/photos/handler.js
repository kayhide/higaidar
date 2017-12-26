'use strict';

const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const path = require('path');
const util = require('util');
const mime = require('mime-types');

const AWS = require('aws-sdk');
const s3 = new AWS.S3();

const getObject = promisify(s3.getObject.bind(s3));
const putObject = promisify(s3.putObject.bind(s3));
const deleteObject = promisify(s3.deleteObject.bind(s3));

const gm = require('gm').subClass({ imageMagick: true });

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');

const srcBucket = `${process.env.RESOURCE_PREFIX}photos`;
const dstBucket = `${process.env.RESOURCE_PREFIX}photos-thumbnail`;

module.exports.accept = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  if (event.Records[0].s3.bucket.name != srcBucket) {
    callback("Source bucket is not right.");
    return;
  }

  const key = event.Records[0].s3.object.key;
  const mimeType = mime.lookup(key);
  if (mimeType != "image/jpeg" && mimeType != "image/png") {
    callback(`Unsupported image type: ${mimeType}`);
    return;
  }

  const userId = parseInt(key.split('/', 1)[0]);
  if (!userId) {
    callback('User id is not detected.');
    return;
  }

  co(function *() {
    const photo = yield model.with(m => co(function *() {
      const user = yield m.User.findById(userId).then(verify.presence);
      const original_url = `${process.env.PHOTOS_LOCATION}${key}`;
      return user.createPhoto({ key, original_url });
    }));

    const res = yield getObject({ Bucket: srcBucket, Key: key });

    const runner = gm(res.Body).autoOrient().resize(200, 200, '^').gravity('Center').extent(200, 200)
    const buffer = yield promisify(runner.toBuffer.bind(runner))(mimeType.split('/')[1]);

    yield putObject({
      Bucket: dstBucket,
      Key: key,
      Body: buffer,
      ContentType: res.ContentType,
      ACL: 'public-read'
    });
    yield model.with(m => co(function *() {
      const thumbnail_url = `${process.env.PHOTOS_THUMBNAIL_LOCATION}${key}`;
      return photo.update({ thumbnail_url });
    }));
  }).then(() => {
    callback(null, 'Photo accepted.');
  }).catch(err => {
    console.log(err);
    callback('Failed to accept photo.');
  });
};


module.exports.index = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const { offset, limit } = Object.assign(
      { offset: 0, limit: 50 },
      _.mapValues(
        _.pick(event.queryStringParameters, 'offset', 'limit'),
        parseInt
      ));
    const data = yield model.with(m => co(function *() {
      return m.Photo.findAndCountAll({ order: [['id', 'DESC']], offset, limit });
    }));

    const items = data.rows.map(item => item.dataValues);
    handleSuccess(callback)(items, { offset, limit, total: data.count });

  }).catch(handleError(callback));
};

module.exports.show = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const data = yield model.with(m => co(function *() {
      return m.Photo.findById(id).then(verify.presence)
    }));

    handleSuccess(callback)(data.dataValues);

  }).catch(handleError(callback));
};

module.exports.update = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const params = JSON.parse(event.body);
    const data = yield model.with(m => co(function *() {
      return m.Photo.findById(id).then(verify.presence)
        .then(u => u.update(params));
    }));

    handleSuccess(callback)(data.dataValues);

  }).catch(handleError(callback));
};

module.exports.destroy = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const data = yield model.with(m => co(function *() {
      const photo = yield m.Photo.findById(id).then(verify.presence)
      yield photo.destroy();
      yield deleteObject({ Bucket: srcBucket, Key: photo.key });
      yield deleteObject({ Bucket: dstBucket, Key: photo.key });
    }));

    handleSuccess(callback)(null);

  }).catch(handleError(callback));
};
