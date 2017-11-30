'use strict';

const co = require('co');
const promisify = require('util.promisify');
const AWS = require('aws-sdk');

const helper = require('app/helper');

let sequelize;

module.exports.hello = (event, context, callback) => {
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    if (!sequelize) {
      sequelize = yield helper.initSequelize();
      console.log('Connection created');
    }
    else {
      console.log('Connection recycled');
    }
    const User = require('app/model/user').define(sequelize);
    const users = yield User.findAll();

    console.log(users.map(u => u.dataValues));
    const response = {
      statusCode: 200,
      body: JSON.stringify({
        message: 'Fine!',
        users: users
      })
    };

    callback(null, response);
  }).catch(err => {
    sequelize = null;
    callback(err);
  });
};
