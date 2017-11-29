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

module.exports.DynamoDB = localize(AWS.DynamoDB, 'localhost:4569');
module.exports.DynamoDB.DocumentClient = localize(AWS.DynamoDB.DocumentClient, 'localhost:4569');
module.exports.S3 = localize(AWS.S3, 'localhost:4572');
module.exports.CloudFormation = localize(AWS.CloudFormation, 'localhost:4581');
