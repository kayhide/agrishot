'use strict';

const fs = require('fs');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const helper = require('../test-helper');
const fixture = require('../fixture');
const Localstack = require('../../lib/localstack');

const awsStub = {
  DynamoDB: Localstack.DynamoDB,
  S3: Localstack.S3,
  '@global': true
}

const s3 = new Localstack.S3();

describe('#photos-thumbnail', () => {
  const srcBucket = 'agrishot-test-photos';
  const dstBucket = `${srcBucket}-thumbnail`;

  let event;
  let handle;

  beforeEach(() => {
    const stub = {
      'aws-sdk': awsStub,
      '@global': true
    };
    const handler = proxyquire('../../app/photos/handler', stub);
    handle = promisify(handler.thumbnail.bind(handler));
  });


  context('with image', () => {
    const file = 'image.jpg';

    beforeEach((done) => {
      event = fixture.read('photos_thumbnail_event');
      helper.upload(s3, srcBucket, file).then(() => done());
    });

    it('creates thumbnail on s3', () => {
      return co(function *() {
        const org = yield helper.exists(s3, dstBucket, file);
        assert(!org);
        yield handle(event, {});
        const cur = yield helper.exists(s3, dstBucket, file);
        assert(cur);
      });
    });
  });

  context('with text', () => {
    const file = 'something.txt';

    beforeEach((done) => {
      event = fixture.read('photos_thumbnail_event_text');
      helper.upload(s3, srcBucket, file).then(() => done());
    });

    it('fails', () => {
      return handle(event, {}).then(
        () => assert(false),
        (err) => {
          assert(err === 'Unsupported image type: txt');
        });
    });
  });
});
