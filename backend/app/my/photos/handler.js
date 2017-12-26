'use strict';

const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const util = require('util');

const AWS = require('aws-sdk');
const s3 = new AWS.S3();

const deleteObject = promisify(s3.deleteObject.bind(s3));

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');

const srcBucket = `${process.env.RESOURCE_PREFIX}photos`;
const dstBucket = `${process.env.RESOURCE_PREFIX}photos-thumbnail`;


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
      const user = yield getUser(m, event).then(verify.presence);
      const where = { user_id: user.id }
      return m.Photo.findAndCountAll({ where, order: [['id', 'DESC']], offset, limit });
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
      const user = yield getUser(m, event).then(verify.presence);
      return m.Photo.findOne({ where: { id: id, user_id: user.id } }).then(verify.presence);
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
      const user = yield getUser(m, event).then(verify.presence);
      return m.Photo.findOne({ where: { id: id, user_id: user.id } }).then(verify.presence)
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
      const user = yield getUser(m, event).then(verify.presence);
      const photo = yield m.Photo.findOne({ where: { id: id, user_id: user.id } }).then(verify.presence);
      yield photo.destroy();
      yield deleteObject({ Bucket: srcBucket, Key: photo.key });
      yield deleteObject({ Bucket: dstBucket, Key: photo.key });
    }));

    handleSuccess(callback)(null);

  }).catch(handleError(callback));
};


const getUser = (m, event) => {
  const id = event.requestContext.authorizer.userId;
  return m.User.findById(id);
};
