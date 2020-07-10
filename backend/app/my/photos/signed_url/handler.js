const _ = require('lodash');
const co = require('co');
const util = require('util');
const promisify = require('util.promisify');
const uuid = require('uuid');
const mime = require('mime-types');
const AWS = require('aws-sdk');

const verify = require('app/verify');
const handleSuccess = require('app/handleSuccess');
const handleError = require('app/handleError');
const model = require('app/model');

module.exports.create = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  co(function* () {
    const params = JSON.parse(event.body);
    const url = yield model.with((m) => co(function* () {
      const user = yield getUser(m, event).then(verify.presence);
      return yield generateSignedUrl(user, params.filename);
    }));

    handleSuccess(callback)({ url });
  }).catch(handleError(callback));
};

const getUser = (m, event) => {
  const id = event.requestContext.authorizer.userId;
  return m.User.findById(id);
};

const s3 = new AWS.S3();
const getSignedUrl = promisify(s3.getSignedUrl.bind(s3));

const generateSignedUrl = (user, filename) => {
  const key = `${user.id}/${uuid.v1().replace(/-/g, '/')}/${filename}`;
  const params = {
    Bucket: `${process.env.RESOURCE_PREFIX}photos`,
    Key: key,
    ContentType: mime.lookup(filename),
    ACL: 'public-read',
  };
  return getSignedUrl('putObject', params);
};
