'use strict';

const co = require('co');
const promisify = require('util.promisify');

const Photo = require('./models/photo');
const Messenger = require('./messenger');
const Predictor = require('./predictor');

const locale = require('./locale');
const t = locale.translations('ja');

module.exports.challenge = (event, context, callback) => {
  const params = event['queryStringParameters'];
  if (params['hub.mode'] === 'subscribe' &&
      params['hub.verify_token'] === 'echo_back_token' ) {
    callback(null, { statusCode: 200, body: params['hub.challenge'] });
  } else {
    callback(null, { statusCode: 403, body: 'Failed validation.' });
  }
};

module.exports.receive = (event, context, callback) => {
  const body = JSON.parse(event['body']);
  const messaging = body.entry[0].messaging[0];
  const senderId = messaging.sender.id;
  const text = messaging.message.text;
  const attachments = messaging.message.attachments;

  co(function *() {
    if (attachments) {
      const src = attachments[0].payload.url;
      const photo = Photo.build({
        src_url: src,
        sender_id: senderId
      });
      yield Photo.create(photo);
      yield Messenger.send(senderId, t.received_image);
    }
    else {
      yield Messenger.send(senderId, t.received_text);
    }
    callback(null, { statusCode: 200 });
  }).catch((err) => {
    console.log(err);
    callback(null, { statusCode: 403, body: err });
  });
};

module.exports.recognize = (event, context, callback) => {
  // console.log(JSON.stringify(event));
  if (event.Records[0].eventName !== 'INSERT') {
    return callback(null, { message: 'Skipping non INSERT event', event });
  }
  co(function *() {
    const data = event.Records[0].dynamodb.NewImage;
    const photo = Photo.unmarshall(data);
    if (!photo.src_url) { throw new Error("`Photo#src_url` is not present") }

    const meta = yield Photo.store(photo);
    yield Photo.update({ id: photo.id, image_url: meta.Location });

    const predictions = yield Predictor.predict(photo.image_url);
    const items = predictions.slice(0, 2).map((item) => `${item.Tag} ${Math.floor(item.Probability * 100)}%`);

    yield Messenger.send(photo.sender_id, t.predictions(items));
    yield Messenger.send(photo.sender_id, t.will_be_in_touch_soon);
    callback(null, { message: 'Recognize successfully called', event });
  }).catch((err) => {
    console.log(err);
    callback(null, { message: 'Recognize failed', event });
  });
};
