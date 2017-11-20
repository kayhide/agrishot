'use strict';

const co = require('co');

const Photo = require('app/models/photo');
const Messenger = require('app/messenger');

const locale = require('app/locale');
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
  const sender = {
    provider: 'facebook',
    id: messaging.sender.id
  };
  const text = messaging.message.text;
  const attachments = messaging.message.attachments;

  co(function *() {
    if (attachments) {
      const src = attachments[0].payload.url;
      const photo = Photo.build({
        src_url: src,
        sender: sender
      });
      yield Photo.create(photo);
      yield Messenger.sendText(sender, t.received_image);
    }
    else {
      yield Messenger.sendText(sender, t.received_text);
    }
    callback(null, { statusCode: 200 });
  }).catch((err) => {
    console.log(err);
    callback(null, { statusCode: 403, body: err });
  });
};
