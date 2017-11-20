const request = require('request');
const promisify = require('util.promisify');


const post = promisify(request.post.bind(request));

module.exports = {
  send(receiver, text, opts = {}) {
    return sendTo[receiver.provider](receiver, text);
  }
}

const sendTo = {
  facebook: (receiver, text) => {
    const args = {
      uri: 'https://graph.facebook.com/v2.6/me/messages',
      headers: {
        'Content-Type': 'application/json'
      },
      json: {
        'recipient': {
          'id': receiver.id
        },
        'message': {
          'text': text
        },
        'access_token': process.env.FACEBOOK_PAGE_ACCESS_TOKEN
      }
    };

    return post(args);
  },

  line: (receiver, text) => {
    const args = {
      uri: 'https://api.line.me/v2/bot/message/reply',
      headers: {
        'Content-Type': 'application/json',
        Authorization: `Bearer ${process.env.LINE_ACCESS_TOKEN}`
      },
      json: {
        replyToken: receiver.line.reply_token,
        messages: [{
          type: 'text',
          text: text
        }]
      }
    };

    return post(args);
  }
}
