const request = require('request');
const promisify = require('util.promisify');

const request_ = promisify(request);

const messenger = {
  facebook: require('app/facebook/messenger'),
  line: require('app/line/messenger')
}

module.exports = {
  sendText(receiver, text) {
    const req = messenger[receiver.provider].reply(receiver, text);
    return request_(req);
  }
}
