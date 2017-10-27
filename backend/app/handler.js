'use strict';

const co = require('co');
const Photo = require('./models/photo');
const Messenger = require('./messenger');

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

  return co(function *() {
    if (attachments) {
      const src = attachments[0].payload.url;
      const photo = new Photo();
      photo.sender_id = senderId;
      const meta = yield photo.store(src)
      photo.image_url = meta.Location;
      photo.image_meta = meta;
      yield photo.save();
      yield Messenger.send(senderId, t.received_image);
      yield Messenger.send(senderId, t.will_be_in_touch_soon);
    }
    else {
      yield Messenger.send(senderId, t.received_text);
    }
  }).then(() => {
    callback(null, { statusCode: 200 });
  }).catch((err) => {
    console.log(err);
    callback(null, { statusCode: 403, body: err });
  });
};
