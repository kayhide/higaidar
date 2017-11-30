'use strict';

const co = require('co');
const promisify = require('util.promisify');
const AWS = require('aws-sdk');

const helper = require('app/helper');

let sequelize;

module.exports.hello = (event, context, callback) => {
  context.callbackWaitsForEmptyEventLoop = false;

  const attempt = (s) => {
    sequelize = s;
    const User = require('app/model/user').define(sequelize);
    return User.findAll();
  };
  const retry = (err) => {
    if (err.name === 'SequelizeAccessDeniedError') {
      console.log('authentication retrying...');
      sequelize = null;
      return helper.initSequelize().then(attempt);
    }
    return Promise.reject(err);
  }

  (sequelize ? Promise.resolve(sequelize) : helper.initSequelize())
    .then(attempt).catch(retry).then((users) => {
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
      console.log(err);
      callback(err);
    });
};
