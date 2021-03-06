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

  co(function* () {
    const params = JSON.parse(event.body);
    const where = {
      code: params.code || null,
      tel: params.tel || null,
    };
    const data = yield model.with(
      (m) => m.User.findOne({ where }).then(verify.authention),
    );
    const user = data.dataValues;
    const token = jwt.sign(
      { user },
      process.env.JWT_SECRET,
      { expiresIn: process.env.JWT_EXPIRES_IN },
    );

    handleSuccess(callback)({ token, user });
  }).catch(handleError(callback));
};

module.exports.authorize = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function* () {
    const m = event.authorizationToken.match(/^Bearer (.+)/);
    if (!m) throw new Error('Bad token');
    const token = m[1];
    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    let effects;
    if (decoded.user.is_admin) {
      effects = [
        ['Allow', ['*/users', '*/users/*']],
        ['Allow', ['*/photos', '*/photos/*']],
        ['Allow', ['*/crops', '*/crops/*']],
        ['Allow', ['*/pests', '*/pests/*']],
        ['Allow', ['*/my/*']],
      ];
    } else if (decoded.user.is_editor) {
      effects = [
        ['Allow', ['*/photos', '*/photos/*']],
        ['Allow', ['*/crops', '*/crops/*']],
        ['Allow', ['*/pests', '*/pests/*']],
        ['Allow', ['*/my/*']],
      ];
    } else {
      effects = [
        ['Allow', ['GET/crops']],
        ['Allow', ['GET/pests']],
        ['Allow', ['*/my/*']],
      ];
    }
    const resource = event.methodArn.replace(/\/.*/, `/${process.env.STAGE}/`);
    const policy = generatePolicy('user', effects, resource);
    policy.context = {
      userId: decoded.user.id,
      userName: decoded.user.name,
    };
    callback(null, policy);
  }).catch((err) => {
    console.log(util.inspect(event, { depth: 5 }));
    console.log(err);
    callback('Unauthorized');
  });
};

const generatePolicy = function (principalId, effects, resource) {
  return {
    principalId,
    policyDocument: {
      Version: '2012-10-17',
      Statement: effects.map(([eff, ress]) => ({
        Action: 'execute-api:Invoke',
        Effect: eff,
        Resource: ress.map((res) => resource + res),
      })),
    },
  };
};
