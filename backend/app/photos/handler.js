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

const Photo = require('app/models/photo');
const Messenger = require('app/messenger');
const Predictor = require('app/predictor');
const locale = require('app/locale');
const t = locale.translations('ja');


module.exports.recognize = (event, context, callback) => {
  // console.log(JSON.stringify(event));
  if (event.Records[0].eventName !== 'INSERT') {
    return callback(null, { message: 'Skipping non INSERT event', event });
  }
  co(function *() {
    const data = event.Records[0].dynamodb.NewImage;
    const photo = Photo.unmarshall(data);
    if (!photo.src_url) { throw new Error("`Photo#src_url` is not present") }

    const meta = yield Messenger.storeImage(photo.sender, photo.src_url, photo.id);
    photo.image_url = meta.Location;
    yield Photo.update(photo);

    const predictions = yield Predictor.predict(photo.image_url);
    const items = predictions.slice(0, 2).map((item) => `${item.Tag} ${Math.floor(item.Probability * 100)}%`);

    yield Messenger.sendText(photo.sender, t.predictions(items));
    yield Messenger.sendText(photo.sender, t.will_be_in_touch_soon);
    callback(null, { message: 'Recognize successfully called', event });
  }).catch((err) => {
    console.log(err);
    callback(null, { message: 'Recognize failed', event });
  });
};

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
