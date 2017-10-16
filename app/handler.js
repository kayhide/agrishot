'use strict';

const co = require('co');
const Photo = require('./models/photo');
const Messenger = require('./messenger');

module.exports.challenge = (event, context, callback) => {
  const params = event['queryStringParameters'];
  if (params['hub.mode'] === 'subscribe' &&
      params['hub.verify_token'] === 'echo_back_token' ) {
    const response = {
      statusCode: 200,
      body: params['hub.challenge']
    };
    callback(null, response);
  } else {
    const response = {
      statusCode: 403,
      body: 'Failed validation.'
    };
    callback(null, response);
  }
};

module.exports.receive = (event, context, callback) => {
  const body = JSON.parse(event['body']);
  const messaging = body.entry[0].messaging[0];
  const senderId = messaging.sender.id;
  const text = messaging.message.text;
  const attachments = messaging.message.attachments;

  return co(function *() {
    yield Messenger.send(senderId, `えっ？${text || '何'}？？`);

    if (attachments) {
      const src = attachments[0].payload.url;
      const photo = new Photo();
      const meta = yield photo.store(src)
      photo.image_url = meta.Location;
      photo.image_meta = meta;
      yield photo.save();
      yield Messenger.send(senderId, `これ？${photo.image_url}`);
    }
  }).then(() => {
    callback(null, { statusCode: 200 });
  }).catch((err) => {
    console.log(err);
    callback(null, { statusCode: 403, body: err });
  });
};
