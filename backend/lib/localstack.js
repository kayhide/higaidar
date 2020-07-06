'use strict';

const AWS = require('aws-sdk');

function localize(base, origin) {
  return class {
    constructor(options = {}) {
      Object.assign(options, {
        region: 'us-east-1',
        endpoint: `http://${origin}`,
        credentials: new AWS.Credentials({
          accessKeyId: '',
          secretAccessKey: ''
        }),
        s3ForcePathStyle: true
      });
      return new base(options);
    }
  };
}

module.exports.S3 = localize(AWS.S3, `localhost:${process.env.S3_PORT}`);
module.exports.CloudFormation = localize(AWS.CloudFormation, `localhost:${process.env.CLOUDFORMATION_PORT}`);
