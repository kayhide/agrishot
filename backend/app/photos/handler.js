'use strict';

const co = require('co');
const promisify = require('util.promisify');
const path = require('path');
const util = require('util');

const AWS = require('aws-sdk');
const s3 = new AWS.S3();
const gm = require('gm').subClass({ imageMagick: true });

const getObject = promisify(s3.getObject.bind(s3));
const putObject = promisify(s3.putObject.bind(s3));

module.exports.thumbnail = (event, context, callback) => {
  // console.log(util.inspect(event, { depth: 5 }));

  const srcBucket = event.Records[0].s3.bucket.name;
  const key = event.Records[0].s3.object.key;
  const dstBucket = `${srcBucket}-thumbnail`;

  if (srcBucket != `${process.env.RESOURCE_PREFIX}photos`) {
    callback("Source bucket is not right.");
    return;
  }

  var imageType = path.extname(key).slice(1);
  if (imageType != "jpg" && imageType != "png") {
    callback(`Unsupported image type: ${imageType}`);
    return;
  }

  co(function *() {
    const res = yield getObject({ Bucket: srcBucket, Key: key });

    const process = gm(res.Body).resize(200, 200, '^').gravity('Center').extent(200, 200)
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
