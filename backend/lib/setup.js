'use strict';

const fs = require('fs');
const path = require('path');
const co = require('co');
const promisify = require('util.promisify');
const sinon = require('sinon');
const _ = require('lodash');

const boot = require('lib/boot');

process.env.STAGE = process.env.STAGE || 'dev';

co(function *() {
  const config = yield boot();
  console.log(`  ${process.env.STAGE} setup`)
  yield createResources(config);
}).catch((err) => {
  console.log('Failed:');
  console.log(err);
});

const createResources = (config) => {
  const Localstack = require('lib/localstack');
  const stackName = `${config.service}-${config.provider.stage}`
  const resources = Object.assign({}, config.resources);
  fixStreamEnabled(resources);
  const template = {
    Resources: resources.Resources,
    Outputs: []
  };
  const params = {
    StackName: stackName,
    TemplateBody: JSON.stringify(template),
    Tags: [{ Key: 'STAGE', Value: config.provider.stage }]
  }

  const cf = new Localstack.CloudFormation();
  return promisify(cf.createStack.bind(cf))(params);
};


const fixStreamEnabled = (resources) => {
  for (let key in resources.Resources) {
    const resource = resources.Resources[key];
    if (resource.Type == 'AWS::DynamoDB::Table') {
      if (resource.Properties.StreamSpecification && resource.Properties.StreamSpecification.StreamViewType) {
        resource.Properties.StreamSpecification.StreamEnabled = true;
      }
    }
  }
};
