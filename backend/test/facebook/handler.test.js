'use strict';

const fs = require('fs');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const proxyquire = require('proxyquire');
const nock = require('nock');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const Localstack = require('lib/localstack');



const awsStub = {
  DynamoDB: Localstack.DynamoDB,
  S3: Localstack.S3,
  '@global': true
}

describe('#facebook-challenge', () => {
  let event;
  let handle;

  beforeEach(() => {
    const stub = {
      'aws-sdk': awsStub
    };
    const handler = proxyquire('app/facebook/handler', stub);
    event = {
      'queryStringParameters': {
        'hub.mode': 'subscribe',
        'hub.verify_token': 'echo_back_token',
        'hub.challenge': 'abcd1234'
      }
    };
    handle = promisify(handler.challenge.bind(handler));
  });

  it('callbacks with success response', () => {
    return handle(event, {}).then((res) => {
      assert(res.statusCode === 200);
      assert(res.body === 'abcd1234');
    });
  });

  context('when hub.mode is not subscribe', () => {
    it('callbacks with failed response', () => {
      event['queryStringParameters']['hub.mode'] = 'bad_mode';
      return handle(event, {}).then((res) => {
        assert(res.statusCode === 403);
      });
    });
  });

  context('when hub.verify_token is not correct', () => {
    it('callbacks with failed response', () => {
      event['queryStringParameters']['hub.verify_token'] = 'bad_token';
      return handle(event, {}).then((res) => {
        assert(res.statusCode === 403);
      });
    });
  });
});


describe('#facebook-receive', () => {
  let event;
  let handle;

  beforeEach(() => {
    const stub = {
      'aws-sdk': awsStub,
      'app/locale/ja': {
        received_text: 'Received text!',
        received_image: 'Received image!',
        will_be_in_touch_soon: 'Will be in touch soon!',
        '@global': true
      }
    };
    const handler = proxyquire('app/facebook/handler', stub);
    handle = promisify(handler.receive.bind(handler));

    nock('https://graph.facebook.com').post(/.*/).reply(200);
  });

  context('with text message', () => {
    beforeEach(() => {
      event = {
        body: JSON.stringify(fixture.read('facebook_receive_event_text'))
      };
    });

    it('callbacks with success response', () => {
      return handle(event, {}).then((res) => {
        assert(res.statusCode === 200);
      });
    });

    it('posts a message to facebook', () => {
      nock.cleanAll();
      const scope = nock('https://graph.facebook.com', { "encodedQueryParams": true })
            .post('/v2.6/me/messages', {
              "recipient": { "id": "6789012345678901" },
              "message": { "text": "Received text!" },
              "access_token": "xxxxxxxxxxxxxxxx"
            }).reply(200);
      return co(function *() {
        yield handle(event, {});
        scope.done();
      });
    });
  });

  context('with image message', () => {
    beforeEach(() => {
      event = {
        body: JSON.stringify(fixture.read('facebook_receive_event_image'))
      };
    });

    it('posts a message to facebook', () => {
      nock.cleanAll();
      const scope = nock('https://graph.facebook.com', { "encodedQueryParams": true })
            .post('/v2.6/me/messages', {
              "recipient": { "id": "6789012345678901" },
              "message": { "text": "Received image!" },
              "access_token": "xxxxxxxxxxxxxxxx"
            }).reply(200);
      return co(function *() {
        yield handle(event, {});
        scope.done();
      });
    });

    it('creates a photo record', () => {
      const Photo = proxyquire('app/models/photo', { 'aws-sdk': awsStub })
      return co(function *() {
        const org = yield Photo.count();
        yield handle(event, {});
        const cur = yield Photo.count();
        assert(cur - org === 1);

        const photo = yield Photo.last();
        assert(photo.sender_id === 'facebook:6789012345678901');
      });
    });
  });
});
