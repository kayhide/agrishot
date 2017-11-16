'use strict';

const co = require('co');
const promisify = require('util.promisify');
const util = require('util');

const AWS = require('aws-sdk');
const s3 = new AWS.S3();
const gm = require('gm').subClass({ imageMagick: true });

module.exports.thumbnail = (event, context, callback) => {
  console.log(util.inspect(event, { depth: 5 }));

  const srcBucket = `${process.env.RESOURCE_PREFIX}photos`;
  if (srcBucket != event.Records[0].s3.bucket.name) {
    callback("Source bucket is not right.");
    return;
  }

  const dstBucket = `${srcBucket}-thumbnail`;

  const key = decodeURIComponent(event.Records[0].s3.object.key.replace(/\+/g, " "));

  var typeMatch = key.match(/\.([^.]*)$/);
  if (!typeMatch) {
    callback("Could not determine the image type.");
    return;
  }
  var imageType = typeMatch[1];
  if (imageType != "jpg" && imageType != "png") {
    callback('Unsupported image type: ${imageType}');
    return;
  }

  const getObject = promisify(s3.getObject.bind(s3));
  const putObject = promisify(s3.putObject.bind(s3));
  co(function *() {
    const res = yield getObject({ Bucket: srcBucket, Key: key });
    const process =
          gm(res.Body)
          .resize(200, 200, '^')
          .gravity('Center')
          .extent(200, 200)
    const buffer = yield promisify(process.toBuffer.bind(process))(imageType);
    yield putObject({
      Bucket: dstBucket,
      Key: key,
      Body: buffer,
      ContentType: res.ContentType,
      ACL: 'public-read'
    });
  }).then(() => {
    callback(null, 'Thumbnail created.');
  }).catch(err => {
    console.log(err);
    callback('Thumbnail failed.');
  });
};
