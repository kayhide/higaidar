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
      const data = yield m.Pest.findAll({ order: [['id', 'ASC']], offset, limit });
      const total = yield m.Pest.count();
      return { data, total }
    }));

    const pests = data.map(u => u.dataValues);
    handleSuccess(callback)(pests, { offset, limit, total });

  }).catch(handleError(callback));
};

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const params = JSON.parse(event.body);
    const data = yield model.with(m => {
      return m.Pest.create(params);
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
      return m.Pest.findById(id).then(verify.presence).then(u => u.destroy());
    });

    handleSuccess(callback)(null);

  }).catch(handleError(callback));
};
