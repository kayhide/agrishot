'use strict';

const url = require('url');
const mime = require('mime-types');
const uuid = require('uuid');
const stream = require('stream');
const request = require('request');
const promisify = require('util.promisify');

const AWS = require('aws-sdk');
const db = new AWS.DynamoDB.DocumentClient();
const s3 = new AWS.S3();


class Photo {
  constructor() {
    this.id = uuid.v1();
    this.created_at = new Date().getTime();
  }

  attributes() {
    return {
      id: this.id,
      image_url: this.image_url,
      image_meta: this.image_meta,
      created_at: this.created_at
    };
  }

  store(location) {
    const key = `${this.id}.jpg`
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

  save() {
    const params = { TableName: `${process.env.RESOURCE_PREFIX}photos`, Item: this.attributes() };
    return promisify(db.put.bind(db))(params);
  }
}

module.exports = Photo;
