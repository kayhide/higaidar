'use strict';

const _ = require('lodash');
const co = require('co');
const util = require('util');
const jwt = require('jsonwebtoken');

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');


module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const params = JSON.parse(event.body);
    const where = {
      code: params.code || null,
      tel: params.tel || null
    }
    const data = yield model.with(['user'], (User) => {
      return User.findOne({ where }).then(verify.authention);
    });
    const token = jwt.sign(
      { user: data.dataValues },
      process.env.JWT_SECRET,
      { expiresIn: process.env.JWT_EXPIRES_IN }
    );

    handleSuccess(callback)({ token });

  }).catch(handleError(callback));
};

module.exports.authorize = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const m = event.authorizationToken.match(/^Bearer (.+)/);
    if (!m) throw new Error('Bad token');
    const token = m[1];
    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    const resource = event.methodArn.replace(/\/.*/, `/${process.env.STAGE}/*`);
    const policy = generatePolicy('user', 'Allow', resource);
    policy.context = {
      userId: decoded.id,
      userName: decoded.name
    }
    callback(null, policy);
  }).catch(err => {
    console.log(util.inspect(event, { depth: 5 }));
    console.log(err);
    callback('Unauthorized');
  });
};

const generatePolicy = function(principalId, effect, resource) {
  return {
    principalId: principalId,
    policyDocument: {
      Version: '2012-10-17',
      Statement: [{
        Action: 'execute-api:Invoke',
        Effect: effect,
        Resource: resource
      }]
    }
  }
}
