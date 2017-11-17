'use strict';

process.env.STAGE = 'test';

const fs = require('fs');
const mime = require('mime');
const co = require('co');
const promisify = require('util.promisify');
const stream = require('stream');

const boot = require('lib/boot');

const fixture = require('test/fixture');

let config;

before((done) => {
  co(function *() {
    config = yield boot();
    done();
  });
});

beforeEach((done) => {
  const helper = require('lib/helper');
  co(function *() {
    const resources = config.resources && config.resources.Resources || [];
    for (let x in resources) {
      yield helper.initResource(resources[x]);
    }
    done();
  });
});


module.exports.upload = (s3, bucket, file) => {
  const pass = stream.PassThrough();
  const params = {
    Bucket: bucket,
    Key: file,
    Body: pass,
    ContentType: mime.lookup(file),
    ACL: 'public-read'
  };
  fs.createReadStream(fixture.join(file)).pipe(pass);
  return promisify(s3.upload.bind(s3))(params)
};


module.exports.exists = (s3, bucket, key) => {
  const params = {
    Bucket: bucket,
    Key: key
  };
  return promisify(s3.headObject.bind(s3))(params)
    .then(() => Promise.resolve(true), () => Promise.resolve(false));
};
