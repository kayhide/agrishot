'use strict';

const fs = require('fs');
const path = require('path');
const co = require('co');
const promisify = require('util.promisify');
const sinon = require('sinon');

const Serverless = require('serverless');
const AWS = require('aws-sdk');
const Localstack = require('./localstack');

module.exports = {
  readConfig() {
    const sls = new Serverless();
    return sls.service.load().then(() => {
      return sls.variables.populateService({ stage: 'test' });
    }).then(() => {
      return Promise.resolve(sls.service);
    });
  },

  createResource(resource) {
    switch (resource.Type) {
    case 'AWS::DynamoDB::Table':
      return this.createTable(resource.Properties);
    case 'AWS::S3::Bucket':
      return this.createBucket(resource.Properties);
    default:
      return Promise.resolve();
    }
  },

  createTable(props) {
    const db = new Localstack.DynamoDB();
    return promisify(db.createTable.bind(db))(props);
  },

  createBucket(props) {
    const s3 = new Localstack.S3();
    const params = { Bucket: props.BucketName };
    return promisify(s3.createBucket.bind(s3))(params);
  },

  initResource(resource) {
    switch (resource.Type) {
    case 'AWS::DynamoDB::Table':
      return this.initTable(resource.Properties);
    case 'AWS::S3::Bucket':
      return this.initBucket(resource.Properties);
    default:
      return Promise.resolve();
    }
  },

  initTable(props) {
    const client = new Localstack.DynamoDB.DocumentClient();
    const params = { TableName: props.TableName }
    const keys = props.KeySchema.map(x => x.AttributeName);
    const scan = promisify(client.scan.bind(client));
    const delete_ = promisify(client.delete.bind(client));
    return co(function *() {
      const data = yield scan({ TableName: props.TableName });

      for (let item of data.Items) {
        let p = Object.assign({
          Key: keys.reduce((o, k) => Object.assign(o, { [k]: item[k] }), {})
        }, params);
        yield delete_(p);
      }
    });
  },

  initBucket(props) {
    const s3 = new Localstack.S3();
    const params = { Bucket: props.BucketName };
    const deleteObject = promisify(s3.deleteObject.bind(s3));
    return co(function *() {
      const data = yield promisify(s3.listObjects.bind(s3))(params);
      for (let item of data.Contents) {
        yield deleteObject(Object.assign({ Key: item.Key }, params));
      }
    });
  }
};
