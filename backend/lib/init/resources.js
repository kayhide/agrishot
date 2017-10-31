'use strict';

const co = require('co');

const helper = require('../../lib/helper');

module.exports.run = () => {
  return co(function *() {
    const config = yield helper.readConfig();
    yield helper.createResources(config);
  });
}
