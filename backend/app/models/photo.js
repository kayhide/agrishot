'use strict';

const co = require('co');
const _ = require('lodash');
const url = require('url');
const mime = require('mime-types');
const uuid = require('uuid');
const stream = require('stream');
const request = require('request');
const promisify = require('util.promisify');

const AWS = require('aws-sdk');
const db = new AWS.DynamoDB.DocumentClient();
const Converter = require('aws-sdk/lib/dynamodb/converter');
const s3 = new AWS.S3();

const Base = require('./base');

class Photo {
  constructor(attrs = {}) {
    Object.assign(this, attrs);
  }

  attributes() {
    return {
      id: this.id,
      src_url: this.src_url,
      sender_id: this.sender_id,
      image_url: this.image_url,
      created_at: this.created_at
    };
  }

  beforeSave() {
    this.id = this.id || uuid.v1();
    this.created_at = this.created_at || new Date().getTime();
  }

  save() {
    return Base.save(this);
  }
}

Photo.tableName = `${process.env.RESOURCE_PREFIX}photos`;

Photo.tableBasename = 'photos';

Photo.build = (attrs = {}) => {
  return new Photo(attrs);
}

Photo.find = (id) => {
  return Base.find(Photo, id);
}

Photo.unmarshall = (data) => {
  return Photo.build(Converter.unmarshall(data));
}

Photo.count = () => {
  return Base.count(Photo);
}

Photo.last = () => {
  return co(function *() {
    const data = yield promisify(db.scan.bind(db))({ TableName: Photo.tableName });
    return _.maxBy(data.Items, 'created_at');
  });
}

Photo.store = (obj, location) => {
  const key = `${obj.id}.jpg`
  const pass = stream.PassThrough();
  const params = {
    Bucket: `${process.env.RESOURCE_PREFIX}photos`,
    Key: key,
    Body: pass,
    ContentType: mime.lookup(url.parse(location).pathname),
    ACL: 'public-read'
  };
  request.get(location).pipe(pass);
  return promisify(s3.upload.bind(s3))(params).then((data) => {
    return Promise.resolve(data);
  });
}


module.exports = Photo;
