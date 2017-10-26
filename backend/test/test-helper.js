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

before((done) => {
  co(function *() {
    const config = yield helper.readConfig();
    initEnv(config.provider.environment);

    const resources = config.resources && config.resources.Resources || [];
    for (let x in resources) {
      yield helper.initResource(resources[x]);
    }
  }).then(() => done());
});
