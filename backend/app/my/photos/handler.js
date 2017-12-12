'use strict';

const _ = require('lodash');
const co = require('co');
const util = require('util');

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');


module.exports.index = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const { offset, limit } = Object.assign(
      { offset: 0, limit: 50 },
      _.pick(event.queryStringParameters, 'offset', 'limit'));
    const { data, total } = yield model.with(m => co(function *() {
      const user = yield getUser(m, event).then(verify.presence);
      const data = yield user.getPhotos({ order: [['id', 'DESC']], offset, limit });
      const total = yield user.countPhotos();
      return { data, total: total || 0 };
    }));

    const items = data.map(u => u.dataValues);
    handleSuccess(callback)(items, { offset, limit, total });

  }).catch(handleError(callback));
};

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const params = JSON.parse(event.body);
    const data = yield model.with(m => co(function *() {
      const user = yield getUser(m, event).then(verify.presence);
      return user.createPhoto(params);
    }));

    handleSuccess(callback)(data.dataValues);

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
      return m.Photo.findOne({ where: { id: id, user_id: user.id } }).then(verify.presence)
        .then(u => u.destroy());
    }));

    handleSuccess(callback)(null);

  }).catch(handleError(callback));
};


const getUser = (m, event) => {
  const id = event.requestContext.authorizer.userId;
  return m.User.findById(id);
};
