'use strict';

const co = require('co');
const helper = require('../lib/helper');

function initEnv(env) {
  let org = Object.assign({}, process.env);

  before(() => {
    Object.assign(process.env, env);
  });

  after(() => {
    process.env = org;
  });
}


let config;

before((done) => {
  co(function *() {
    config = yield helper.readConfig('test');
    initEnv(config.provider.environment);
    done();
  });
});

beforeEach((done) => {
  co(function *() {
    const resources = config.resources && config.resources.Resources || [];
    for (let x in resources) {
      yield helper.initResource(resources[x]);
    }
    done();
  });
});
