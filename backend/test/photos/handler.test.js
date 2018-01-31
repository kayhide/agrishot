'use strict';

const _ = require('lodash');
const fs = require('fs');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');
const nock = require('nock');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const Localstack = require('lib/localstack');


const s3 = new helper.awsStub.S3();

describe('#photos-recognize', () => {
  let event;
  let handle;
  let messenger;
  let predictor;
  let locale;

  beforeEach(() => {
    messenger = {
      replyTexts: sinon.stub().returns(Promise.resolve())
    }
    predictor = {
      predict: sinon.stub().returns(Promise.resolve([
        {
          "TagId": "d3398b08-0dec-4ba1-a361-3da98d6bb92d",
          "Tag": "Mikan Sabi Dani",
          "Probability": 0.9801368
        },
        {
          "TagId": "ef9786bc-71b0-4bf7-8173-ee7eb9e813f5",
          "Tag": "Chano Hokori Dani",
          "Probability": 0.318256438
        },
        {
          "TagId": "570ed067-e870-46a7-a381-1dfe94a31d7b",
          "Tag": "Kokuten Byo",
          "Probability": 0.261666328
        }
      ]))
    }
    const stub = {
      'aws-sdk': helper.awsStub,
      'app/messenger': messenger,
      'app/predictor': predictor,
      'app/locale/ja': {
        received_text: 'Received text!',
        received_image: 'Received image!',
        might_be_wrong: 'Might be wrong...',
        contact_here: (id) => "Contact here: line://home/public/main?id=" + id,
        shop_site_here: (url) => "Shop site here: " + url,
        details_here: (url) => "Details here: " + url,
        predictions: (items) => ["Predictions:", ...items].join("\n"),
        '@global': true
      }
    };
    const handler = proxyquire('app/photos/handler', stub);
    handle = promisify(handler.recognize.bind(handler));
  });

  context('with a new photo records', () => {
    beforeEach(() => {
      event = fixture.read('photos_recognize_event');
      nock('https://agrishot.test')
        .get('/path/to/image.jpg?xxx=abcdef')
        .reply(200, (uri, requestBody) => fs.createReadStream(fixture.join('image.jpg')));
    });

    it('calls messenger.replyTexts with a text', () => {
      return handle(event, {}).then((res) => {
        assert(messenger.replyTexts.calledOnce);
        assert(messenger.replyTexts.getCall(0).args[0].provider === 'facebook');
        assert(messenger.replyTexts.getCall(0).args[0].id  === '6789012345678901');
        assert(_.isEqual(
          messenger.replyTexts.getCall(0).args[1],
          [
            'Predictions:\nMikan Sabi Dani 98%\nChano Hokori Dani 31%',
            'Might be wrong...',
            'Contact here: line://home/public/main?id=atid1234',
            'Shop site here: http://shop.agrishot.test/',
          ]
        ));
      });
    });

    it('replys with pests description and url', () => {
      const Pest = proxyquire('app/models/pest', { 'aws-sdk': helper.awsStub })
      return co(function *() {
        const pest = Pest.create({
          label: 'Mikan Sabi Dani',
          description: 'This mikan is sabiing and daniing.',
          url: 'http://agrishot.test/msd/'
        });

        const res = yield handle(event, {});
        assert(_.isEqual(
          messenger.replyTexts.getCall(0).args[1],
          [
            'Predictions:\nMikan Sabi Dani 98%\nChano Hokori Dani 31%',
            'This mikan is sabiing and daniing.',
            'Details here: http://agrishot.test/msd/',
          ]
        ));
      });
    });

    it('calls predictor.predict with image_url', () => {
      return handle(event, {}).then((res) => {
        assert(predictor.predict.calledOnce);
        assert(predictor.predict.getCall(0).args[0].startsWith('http://localhost:4572/agrishot-test-photos/'));
      });
    });

    it('stores image to bucket', () => {
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

    it('updates photo record', () => {
      const Photo = proxyquire('app/models/photo', { 'aws-sdk': helper.awsStub })
      return co(function *() {
        const org = Photo.unmarshall(event.Records[0].dynamodb.NewImage);
        yield handle(event, {});
        const cur = yield Photo.find(org.id);

        assert(org.image_url === undefined);
        assert(cur.image_url.startsWith('http://localhost:4572/agrishot-test-photos/'));
      });
    });
  });
});


describe('#photos-thumbnail', () => {
  const srcBucket = 'agrishot-test-photos';
  const dstBucket = `${srcBucket}-thumbnail`;

  let event;
  let handle;

  beforeEach(() => {
    const handler = proxyquire('app/photos/handler', { 'aws-sdk': helper.awsStub });
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
