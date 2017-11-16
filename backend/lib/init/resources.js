'use strict';

const co = require('co');
const boot = require('../boot');

const helper = require('../helper');

module.exports.run = () => {
  return co(function *() {
    const config = yield boot();
    yield helper.createResources(config);
  });
}
