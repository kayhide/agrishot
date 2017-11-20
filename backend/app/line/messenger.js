const _ = require('lodash');

module.exports = {
  sendText: (receiver, text) => {
    throw new Error('Push message is not ready for LINE');
  },

  reply: (receiver, texts) => {
    return [{
      method: 'POST',
      uri: 'https://api.line.me/v2/bot/message/reply',
      headers: {
        'Content-Type': 'application/json',
        Authorization: `Bearer ${process.env.LINE_ACCESS_TOKEN}`
      },
      json: {
        replyToken: receiver.line.reply_token,
        messages: texts.map(t => { return { type: 'text', text: t } })
      }
    }];
  },

  get: (url) => {
    return {
      method: 'GET',
      uri: url,
      headers: {
        Authorization: `Bearer ${process.env.LINE_ACCESS_TOKEN}`
      }
    };
  }
};
