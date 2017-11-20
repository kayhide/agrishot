const request = require('request');
const promisify = require('util.promisify');
const url = require('url');
const path = require('path');
const mime = require('mime-types');
const stream = require('stream');

const AWS = require('aws-sdk');
const s3 = new AWS.S3();

const request_ = promisify(request);

const messenger = {
  facebook: require('app/facebook/messenger'),
  line: require('app/line/messenger')
}

module.exports = {
  sendText(receiver, text) {
    const req = messenger[receiver.provider].reply(receiver, text);
    return request_(req);
  },

  storeImage(sender, src, basename) {
    const req = messenger[sender.provider].get(src);
    const pass = stream.PassThrough();
    const pathname = url.parse(src).pathname;
    const key = `${basename}${path.extname(pathname) || '.jpg'}`

    const params = {
      Bucket: `${process.env.RESOURCE_PREFIX}photos`,
      Key: key,
      Body: pass,
      ContentType: mime.lookup(key),
      ACL: 'public-read'
    };
    request(req).pipe(pass);
    return promisify(s3.upload.bind(s3))(params);
  }
}
