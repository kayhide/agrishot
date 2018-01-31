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


const tableName = `${process.env.RESOURCE_PREFIX}pests`;

const model = dynogels.define('Pest', {
  tableName: tableName,
  hashKey: 'id',
  schema: {
    id: Joi.string().uuid().required(),
    label: Joi.string(),
    description: Joi.string(),
    url: Joi.string()
  }
});


const Pest = {
  _model: model,

  build: (attrs = {}) => {
    return new model(attrs).attrs;
  },

  unmarshall: (data) => {
    return Pest.build(Converter.unmarshall(data));
  },

  create: (attrs = {}) => {
    const attrs_ = Object.assign({
      id: uuid.v1()
    }, attrs);
    return promisify(model.create)(attrs_).then(() => attrs_);
  },

  update: (attrs = {}) => {
    const attrs_ = attrs;
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
  }
};


module.exports = Pest;
