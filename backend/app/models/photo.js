'use strict';

const co = require('co');
const _ = require('lodash');
const uuid = require('uuid');
const promisify = require('util.promisify');

const Joi = require('joi');
const dynogels = require('dynogels');


const AWS = require('aws-sdk');
const Converter = require('aws-sdk/lib/dynamodb/converter');
const s3 = new AWS.S3();


const tableName = `${process.env.RESOURCE_PREFIX}photos`;
const PART = 0;

const model = dynogels.define('Photo', {
  tableName: tableName,
  hashKey: 'id',
  schema: {
    id: Joi.string().uuid().required(),
    sender_id: Joi.string().required(),
    src_url: Joi.string(),
    image_url: Joi.string(),
    created_at: Joi.number().required(),
    updated_at: Joi.number().required(),
    sender: {
      provider: Joi.string(),
      id: Joi.string(),
      line: {
        reply_token: Joi.string()
      }
    },
    part: Joi.number().required()
  },

  indexes: [{
    name: `${tableName}-part-created_at`,
    type: 'global',
    hashKey: 'part',
    rangeKey: 'created_at'
  }]
});


function injectSenderId(attrs) {
  if (attrs.sender && attrs.sender.provider && attrs.sender.id) {
    attrs.sender_id = `${attrs.sender.provider}:${attrs.sender.id}`;
  }
  else {
    attrs.sender_id = undefined;
  }
}

function queryByCreatedAt(f) {
  const q = f(model.query(PART).usingIndex(`${tableName}-part-created_at`));
  return promisify(q.exec.bind(q))().then(res => _.map(res.Items, 'attrs'));
}

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
      updated_at: now,
      part: PART
    }, attrs);
    injectSenderId(attrs_)
    return promisify(model.create)(attrs_).then(() => attrs_);
  },

  update: (attrs = {}) => {
    const now = Date.now();
    const attrs_ = Object.assign({}, attrs, {
      updated_at: now
    });
    injectSenderId(attrs_)
    return promisify(model.update)(attrs_).then(() => attrs_);
  },

  find: (id) => {
    return promisify(model.get)(id).then(item => item ? item.attrs : undefined);
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

  first: () => {
    return queryByCreatedAt(q => q.ascending().limit(1)).then(res => res[0]);
  },

  last: () => {
    return queryByCreatedAt(q => q.descending().limit(1)).then(res => res[0]);
  },

  olderFirst: () => {
    return queryByCreatedAt(q => q.ascending());
  },

  newerFirst: () => {
    return queryByCreatedAt(q => q.descending());
  }
};


module.exports = Photo;
