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

const gm = require('gm').subClass({ imageMagick: true });

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');

const srcBucket = `${process.env.RESOURCE_PREFIX}photos`;
const dstBucket = `${process.env.RESOURCE_PREFIX}photos-thumbnail`;

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const photo = yield model.with(m => co(function *() {
      return m.Photo.findById(id).then(verify.presence)
    }));

    const key = photo.key;
    const res = yield getObject({ Bucket: srcBucket, Key: key });

    const runner = gm(res.Body).autoOrient().resize(200, 200, '^').gravity('Center').extent(200, 200)
    const buffer = yield promisify(runner.toBuffer.bind(runner))(res.ContentType.split('/')[1]);

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

    handleSuccess(callback)(photo.dataValues);

  }).catch(handleError(callback));
};
