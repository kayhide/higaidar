'use strict';

const co = require('co');
const util = require('util');

const model = require('app/model');


module.exports.index = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const data = yield model.with([`user`], (User) => {
      return User.findAll();
    });

    const users = data.map(u => u.dataValues);
    const response = {
      statusCode: 200,
      body: JSON.stringify(users)
    };
    callback(null, response);
  }).catch(err => {
    console.log(err);
    callback(err);
  });
};

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const body = JSON.parse(event.body);
    const data = yield model.with([`user`], (User) => {
      return User.create(body);
    });

    const response = {
      statusCode: 200,
      body: JSON.stringify(data.dataValues)
    };
    callback(null, response);
  }).catch(err => {
    console.log(err);
    callback(err);
  });
};
