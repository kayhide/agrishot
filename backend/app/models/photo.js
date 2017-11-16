'use strict';

const co = require('co');
const _ = require('lodash');
const url = require('url');
const mime = require('mime-types');
const uuid = require('uuid');
const stream = require('stream');
const request = require('request');
const promisify = require('util.promisify');

const Joi = require('joi');
const dynogels = require('dynogels');


const AWS = require('aws-sdk');
const Converter = require('aws-sdk/lib/dynamodb/converter');
const s3 = new AWS.S3();


const model = dynogels.define('Photo', {
  hashKey: 'id',

  schema: {
    id: Joi.string().uuid(),
    sender_id: Joi.string(),
    src_url: Joi.string(),
    image_url: Joi.string(),
    created_at: Joi.number(),
    updated_at: Joi.number()
  },

  tableName: `${process.env.RESOURCE_PREFIX}photos`
});


const Photo = {
  _model: model,

  build: (attrs = {}) => {
    return new model(attrs).attrs;
  },

  unmarshall: (data) => {
    return Photo.build(Converter.unmarshall(data));
  },

  create: (attrs = {}) => {
    const now = Date.now();
    const attrs_ = Object.assign({
      id: uuid.v1(),
      created_at: now,
      updated_at: now
    }, attrs);
    return promisify(model.create)(attrs_).then(() => attrs_);
  },

  update: (attrs = {}) => {
    const attrs_ = Object.assign({}, attrs);
    attrs_.updated_at = Date.now();
    return promisify(model.update)(attrs_).then(() => attrs_);
  },

  find: (id) => {
    return promisify(model.get)(id).then(item => item.attrs);
  },

  count: () => {
    const q = model.scan().select('COUNT');
    return promisify(q.exec.bind(q))()
      .then(res => res.Count);
  },

  all: () => {
    const q = model.scan();
    return promisify(q.exec.bind(q))()
      .then(res => res.Items.map(item => item.attrs));
  },

  last: () => {
    const q = model.scan();
    return promisify(q.exec.bind(q))()
      .then(res => _.maxBy(res.Items, item => item.attrs.created_at))
      .then(item => item.attrs);
  },

  store: (obj) => {
    const key = `${obj.id}.jpg`
    const location = obj.src_url;
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
};


module.exports = Photo;
