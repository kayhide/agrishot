'use strict';

const co = require('co');

const helper = require('lib/helper');

module.exports.run = (config) => {
  return co(function *() {
    yield helper.createResources(config);
  });
}
