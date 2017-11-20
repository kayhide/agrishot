'use strict';

const fs = require('fs');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');
const nock = require('nock');

const helper = require('test/test-helper');
const fixture = require('test/fixture');


describe('#line-receive', () => {
  let event;
  let handle;

  beforeEach(() => {
    const stub = {
      'aws-sdk': helper.awsStub,
      'app/locale/ja': {
        received_text: 'Received text!',
        received_image: 'Received image!',
        will_be_in_touch_soon: 'Will be in touch soon!',
        '@global': true
      }
    };
    const handler = proxyquire('app/line/handler', stub);
    handle = promisify(handler.receive.bind(handler));

    nock('https://api.line.me').post(/.*/).reply(200);
  });

  context('with text message', () => {
    beforeEach(() => {
      event = fixture.read('line_receive_event_text');
    });

    it('callbacks with success response', () => {
      return handle(event, {}).then((res) => {
        assert(res.statusCode === 200);
      });
    });

    it('posts a message to line', () => {
      nock.cleanAll();
      const scope =
            nock('https://api.line.me', { "encodedQueryParams":true })
            .post('/v2/bot/message/reply', {
              "replyToken": "replytokenxxxxxxxxxxxxxxxxxxxxxx",
              "messages": [{ "type":"text", "text":"Received text!" }]
            }).reply(200);

      return co(function *() {
        yield handle(event, {});
        scope.done();
      });
    });
  });

  context('with image message', () => {
    beforeEach(() => {
      event = fixture.read('line_receive_event_image');
    });

    it('callbacks with success response', () => {
      return handle(event, {}).then((res) => {
        assert(res.statusCode === 200);
      });
    });

    it('creates a photo record', () => {
      const Photo = proxyquire('app/models/photo', { 'aws-sdk': helper.awsStub })
      return co(function *() {
        const org = yield Photo.count();
        yield handle(event, {});
        const cur = yield Photo.count();
        assert(cur - org === 1);

        const photo = yield Photo.last();
        assert(photo.sender_id === 'line:Uffffffffffffffffffffffffffffffff');
      });
    });
  });
});
