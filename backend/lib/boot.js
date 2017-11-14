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
  Object.assign(process.env, {
    AWS_REGION: config.provider.region,
    AWS_PROFILE: config.provider.profile
  });
}

module.exports = function() {
  return readConfig().then(conf => {
    injectEnv(conf)
    return Promise.resolve(conf);
  });
};
