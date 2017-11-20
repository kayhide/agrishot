module.exports = {
  sendText: (receiver, text) => {
    return {
      method: 'POST',
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
  },

  reply: (receiver, texts) => {
    return texts.map(t => this.sendText(receiver, t));
  },

  get: (url) => {
    return {
      method: 'GET',
      uri: url
    };
  }
};
