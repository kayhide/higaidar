const _ = require('lodash');
const co = require('co');
const util = require('util');

const verify = require('app/verify');
const parseParams = require('app/parseParams');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');

module.exports.index = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function* () {
    const params = parseParams(event);
    const { offset, limit } = params.pager({ offset: 0, limit: 50 });
    const data = yield model.with((m) => co(function* () {
      return m.Crop.findAndCountAll({ order: [['id', 'ASC']], offset, limit });
    }));

    const items = data.rows.map((item) => item.dataValues);
    handleSuccess(callback)(items, { offset, limit, total: data.count });
  }).catch(handleError(callback));
};

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function* () {
    const params = JSON.parse(event.body);
    const data = yield model.with((m) => m.Crop.create(params));

    handleSuccess(callback)(data.dataValues);
  }).catch(handleError(callback));
};

module.exports.destroy = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function* () {
    const { id } = event.pathParameters;
    const data = yield model.with(
      (m) => m.Crop.findById(id).then(verify.presence).then(
        (u) => u.destroy(),
      ),
    );

    handleSuccess(callback)(null);
  }).catch(handleError(callback));
};
