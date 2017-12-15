'use strict';

const co = require('co');
const promisify = require('util.promisify');
const path = require('path');
const util = require('util');

const AWS = require('aws-sdk');
const s3 = new AWS.S3();

const getObject = promisify(s3.getObject.bind(s3));
const putObject = promisify(s3.putObject.bind(s3));

const gm = require('gm').subClass({ imageMagick: true });

const verify = require('app/verify');
const model = require('app/model');


module.exports.accept = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  context.callbackWaitsForEmptyEventLoop = false;

  const srcBucket = event.Records[0].s3.bucket.name;
  const key = event.Records[0].s3.object.key;
  const dstBucket = `${srcBucket}-thumbnail`;

  if (srcBucket != `${process.env.RESOURCE_PREFIX}photos`) {
    callback("Source bucket is not right.");
    return;
  }

  var imageType = path.extname(key).slice(1);
  if (imageType != "jpg" && imageType != "png") {
    callback(`Unsupported image type: ${imageType}`);
    return;
  }

  const userId = parseInt(key.split('/', 1)[0]);
  if (!userId) {
    callback('User id is not detected.');
    return;
  }

  co(function *() {
    const photo = yield model.with(m => co(function *() {
      const user = yield m.User.findById(userId).then(verify.presence);
      const original_url = `${process.env.PHOTOS_LOCATION}${key}`;
      return yield user.createPhoto({ key, original_url });
    }));

    const res = yield getObject({ Bucket: srcBucket, Key: key });

    const runner = gm(res.Body).resize(200, 200, '^').gravity('Center').extent(200, 200)
    const buffer = yield promisify(runner.toBuffer.bind(runner))(imageType);

    yield putObject({
      Bucket: dstBucket,
      Key: key,
      Body: buffer,
      ContentType: res.ContentType,
      ACL: 'public-read'
    });
    yield model.with(m => co(function *() {
      const thumbnail_url = `${process.env.PHOTOS_THUMBNAIL_LOCATION}${key}`;
      return yield photo.update({ thumbnail_url });
    }));
  }).then(() => {
    callback(null, 'Photo accepted.');
  }).catch(err => {
    console.log(err);
    callback('Failed to accept photo.');
  });
};
