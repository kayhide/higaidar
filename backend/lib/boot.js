'use strict';

const Serverless = require('serverless');

function readConfig() {
  const sls = new Serverless();
  return sls.service.load().then(() => {
    return sls.variables.populateService();
  }).then(() => {
    return Promise.resolve(sls.service);
  });
}

function injectEnv(config) {
  Object.assign(process.env, config.provider.environment);
  Object.assign(process.env, config.custom.env);
  Object.assign(process.env, {
    STAGE: config.provider.stage,
    AWS_REGION: config.provider.region,
    AWS_PROFILE: config.provider.profile
  });
}

let config;

module.exports = function() {
  if (config) {
    return Promise.resolve(config);
  }
  else {
    return readConfig().then(c => {
      config = c;
      injectEnv(c)
      return c;
    });
  }
};
