'use strict';

process.env.STAGE = 'test';

const co = require('co');

const boot = require('../lib/boot');

let config;

before((done) => {
  co(function *() {
    config = yield boot();
    done();
  });
});

beforeEach((done) => {
  const helper = require('../lib/helper');
  co(function *() {
    const resources = config.resources && config.resources.Resources || [];
    for (let x in resources) {
      yield helper.initResource(resources[x]);
    }
    done();
  });
});
