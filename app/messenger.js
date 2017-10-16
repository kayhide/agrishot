const request = require('request');
const promisify = require('util.promisify');

module.exports = {
  send(recipientId, text) {
    const options = {
      uri: 'https://graph.facebook.com/v2.6/me/messages',
      headers: {
        'Content-Type': 'application/json'
      },
      json: {
        'recipient': {
          'id': recipientId
        },
        'message': {
          'text': text
        },
        'access_token': process.env.FACEBOOK_PAGE_ACCESS_TOKEN
      }
    };

    return promisify(request.post.bind(request))(options);
  }
}
