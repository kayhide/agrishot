'use strict';

const fs = require('fs');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');
const nock = require('nock');

const helper = require('./test-helper');
const fixture = require('./fixture');
const Localstack = require('../lib/localstack');

const awsStub = {
  DynamoDB: Localstack.DynamoDB,
  S3: Localstack.S3,
  '@global': true
}

describe('#challenge', () => {
  let event;
  let handle;

  beforeEach(() => {
    const stub = {
      'aws-sdk': awsStub
    };
    const handler = proxyquire('../app/handler', stub);
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


describe('#receive', () => {
  let event;
  let handle;
  let messenger;
  let locale;

  beforeEach(() => {
    messenger = {
      send: sinon.stub().returns(Promise.resolve())
    }
    const stub = {
      'aws-sdk': awsStub,
      './messenger': messenger,
      './locale/ja': {
        received_text: 'Received text!',
        received_image: 'Received image!',
        will_be_in_touch_soon: 'Will be in touch soon!',
        '@global': true
      }
    };
    const handler = proxyquire('../app/handler', stub);
    handle = promisify(handler.receive.bind(handler));
  });

  context('with text message', () => {
    beforeEach(() => {
      event = {
        body: JSON.stringify(fixture.read('receive_event'))
      };
    });

    it('callbacks with success response', () => {
      return handle(event, {}).then((res) => {
        assert(res.statusCode === 200);
      });
    });

    it('calls messenger.send with locale.ja.received_text', () => {
      return handle(event, {}).then((res) => {
        assert(messenger.send.calledOnce);
        assert(messenger.send.getCall(0).args[0] === '6789012345678901');
        assert(messenger.send.getCall(0).args[1] === 'Received text!');
      });
    });
  });

  context('with image message', () => {
    beforeEach(() => {
      event = {
        body: JSON.stringify(fixture.read('receive_event_image'))
      };
      nock('https://agrishot.test')
        .get('/path/to/image.jpg?xxx=abcdef')
        .reply(200, (uri, requestBody) => fs.createReadStream(fixture.join('image.jpg')));
    });

    it('calls messenger.send twice with some text and with image url', () => {
      const db = new Localstack.DynamoDB.DocumentClient();
      return co(function *() {
        yield handle(event, {});
        assert(messenger.send.calledTwice);
        assert(messenger.send.getCall(0).args[0] === '6789012345678901');
        assert(messenger.send.getCall(0).args[1] === 'Received image!');
        assert(messenger.send.getCall(1).args[0] === '6789012345678901');
        assert(messenger.send.getCall(1).args[1] === 'Will be in touch soon!');
      });
    });

    it('creates photo record', () => {
      const params = { TableName: 'agrishot-test-photos', Select: 'COUNT' };
      const Photo = proxyquire('../app/models/photo', { 'aws-sdk': awsStub })
      return co(function *() {
        const org = yield Photo.count();
        yield handle(event, {});
        const cur = yield Photo.count();
        assert(cur - org === 1);

        const photo = yield Photo.last();
        assert(photo.image_url.startsWith('http://localhost:4572/agrishot-test-photos/'));
        assert(photo.sender_id === '6789012345678901');
      });
    });

    it('stores image to bucket', () => {
      const s3 = new Localstack.S3();
      const params = { Bucket: 'agrishot-test-photos' };

      return co(function *() {
        const org = yield promisify(s3.listObjects.bind(s3))(params);
        yield handle(event, {});
        const items = yield promisify(s3.listObjects.bind(s3))(params);
        assert(items.Contents.length - org.Contents.length === 1);

        const key = items.Contents[items.Contents.length - 1].Key;
        const item = yield promisify(s3.headObject.bind(s3))(Object.assign({ Key: key }, params));
        assert(item.ContentType === 'image/jpeg');

        const acl = yield promisify(s3.getObjectAcl.bind(s3))(Object.assign({ Key: key }, params));
        const pr = acl.Grants.find((g) =>
                                   g.Grantee.URI === 'http://acs.amazonaws.com/groups/global/AllUsers' &&
                                   g.Permission === 'READ');
        assert(pr);
      });
    });
  });
});
