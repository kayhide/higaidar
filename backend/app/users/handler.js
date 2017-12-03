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
    const params = JSON.parse(event.body);
    const data = yield model.with([`user`], (User) => {
      return User.create(params);
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

module.exports.show = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const data = yield model.with([`user`], (User) => {
      return User.findById(id);
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

module.exports.update = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const params = JSON.parse(event.body);
    const data = yield model.with([`user`], (User) => {
      return User.findById(id).then(u => u.update(params));
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

module.exports.destroy = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const id = event.pathParameters.id;
    const data = yield model.with([`user`], (User) => {
      return User.findById(id).then(u => u.destroy());
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
