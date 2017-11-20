'use strict';

const co = require('co');
const util = require('util');

const Photo = require('app/models/photo');
const Messenger = require('app/messenger');

const locale = require('app/locale');
const t = locale.translations('ja');


module.exports.receive = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));
  const body = JSON.parse(event['body']);
  const e = body.events[0];
  const sender = {
    provider: 'line',
    id: e.source.userId,
    line: {
      reply_token: e.replyToken
    }
  };
  const message = e.message;

  co(function *() {
    yield respondOn(message)(sender, message);
    callback(null, { statusCode: 200 });
  }).catch((err) => {
    console.log(err);
    callback(null, { statusCode: 403, body: err });
  });
};

const respondOn = (message) => {
  return responder[message.type] ||
    ((sender, message) => Promise.reject(`Unacceptable message type: ${message.type}`));
};

const responder = {
  text: (sender, message) => {
    return Messenger.send(sender, t.received_text);
  },

  image: (sender, message) => {
    return co(function *() {
      const src = `https://api.line.me/v2/bot/message/${message.id}/content`;
      const photo = Photo.build({
        src_url: src,
        sender: sender
      });
      yield Photo.create(photo);
      yield Messenger.send(sender, t.received_image);
    });
  }
};
