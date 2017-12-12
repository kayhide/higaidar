'use strict';

const Localstack = require('lib/localstack');

module.exports.stub = {
  'aws-sdk': {
    DynamoDB: Localstack.DynamoDB,
    S3: Localstack.S3,
    '@global': true
  }
};
