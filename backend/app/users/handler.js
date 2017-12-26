'use strict';

const _ = require('lodash');
const co = require('co');
const util = require('util');

const verify = require('app/verify');
const parseFilter = require('app/parseFilter');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');


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
    const where = parseFilter(_.pick(event.queryStringParameters, 'id'));
    const data = yield model.with(m => co(function *() {
      return m.User.findAndCountAll({ order: [['code', 'ASC']], offset, limit, where });
    }));

    const users = data.rows.map(item => item.dataValues);
    handleSuccess(callback)(users, { offset, limit, total: data.count });

  }).catch(handleError(callback));
};

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const params = JSON.parse(event.body);
    const data = yield model.with(m => {
      return m.User.create(params).then(user => m.User.findById(user.id));
    });

    handleSuccess(callback)(data.dataValues);

  }).catch(handleError(callback));
};

module.exports.show = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const data = yield model.with(m => {
      return m.User.findById(id).then(verify.presence);
    });

    handleSuccess(callback)(data.dataValues);

  }).catch(handleError(callback));
};

module.exports.update = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const params = JSON.parse(event.body);
    const data = yield model.with(m => {
      return m.User.findById(id).then(verify.presence).then(u => u.update(params));
    });

    handleSuccess(callback)(data.dataValues);

  }).catch(handleError(callback));
};

module.exports.destroy = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const data = yield model.with(m => {
      return m.User.findById(id).then(verify.presence).then(u => u.destroy());
    });

    handleSuccess(callback)(null);

  }).catch(handleError(callback));
};
