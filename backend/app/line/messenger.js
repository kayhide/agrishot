module.exports = {
  reply: (receiver, text) => {
    return {
      method: 'POST',
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
