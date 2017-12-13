'use strict';

const _ = require('lodash');
const co = require('co');
const util = require('util');
const promisify = require('util.promisify');
const uuid = require('uuid');
const AWS = require('aws-sdk');

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');


module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    const params = JSON.parse(event.body);
    const data = yield model.with(m => co(function *() {
      const user = yield getUser(m, event).then(verify.presence);
      return yield generatePresignedPost(user, params.filename);
    }));

    handleSuccess(callback)(data);

  }).catch(handleError(callback));
};


const getUser = (m, event) => {
  const id = event.requestContext.authorizer.userId;
  return m.User.findById(id);
};


const s3 = new AWS.S3();
const createPresignedPost = promisify(s3.createPresignedPost.bind(s3));

const generatePresignedPost = (user, filename) => {
  const key = `${user.id}/${uuid.v1().replace(/-/g, '/')}/${filename}`
  const params = {
    Bucket: `${process.env.RESOURCE_PREFIX}photos`,
    Fields: { key }
  };
  return createPresignedPost(params);
};
