'use strict';

const co = require('co');
const boot = require('lib/boot');

const helper = require('lib/helper');

module.exports.run = () => {
  return co(function *() {
    const config = yield boot();
    yield helper.createResources(config);
  });
}
