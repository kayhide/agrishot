'use strict';

const fs = require('fs');
const path = require('path');
const sinon = require('sinon');
const promisify = require('util.promisify');

const Serverless = require('serverless');
const AWS = require('aws-sdk');
const Localstack = require('./localstack');

module.exports.fixture = {
  read(name) {
    return JSON.parse(fs.readFileSync(path.join(__dirname, 'fixtures', `${name}.json`), 'utf8'));
  },

  join(name) {
    return path.join(__dirname, 'fixtures', name);
  }
}

function readConfig() {
  const sls = new Serverless();
  return sls.service.load().then(() => {
    return sls.variables.populateService({ stage: 'test' });
  }).then(() => {
    return Promise.resolve(sls.service);
  });
}

function initResource(resource) {
  switch (resource.Type) {
  case 'AWS::DynamoDB::Table':
    return initTable(resource.Properties);
  case 'AWS::S3::Bucket':
    return initBucket(resource.Properties);
  default:
    return Promise.resolve();
  }
}

function initTable(props) {
  const db = new Localstack.DynamoDB();
  const createTable = promisify(db.createTable.bind(db));
  const deleteTable = promisify(db.deleteTable.bind(db));
  return deleteTable({TableName: props.TableName})
    .catch(() => {})
    .then(() => createTable(props));
}

function initBucket(props) {
  const s3 = new Localstack.S3();
  const params = { Bucket: props.BucketName };
  return promisify(s3.headBucket.bind(s3))(params).then((data) => {
    return emptyBucket(s3, props.BucketName);
  }, (err) => {
    return promisify(s3.createBucket.bind(s3))(params);
  });
}

function emptyBucket(s3, bucket) {
  const params = { Bucket: bucket };
  return promisify(s3.listObjects.bind(s3))(params).then((data) => {
    const ps = data.Contents.map((item) => {
      return promisify(s3.deleteObject.bind(s3))(Object.assign({ Key: item.Key }, params));
    });
    return Promise.all(ps);
  });
}

function initEnv(env) {
  let org = Object.assign({}, process.env);

  before(() => {
    Object.assign(process.env, env);
  });

  after(() => {
    process.env = org;
  });
}

before((done) => {
  readConfig().then((config) => {
    initEnv(config.provider.environment);
    const resources = config.resources && config.resources.Resources || [];
    const ps = Object.keys(resources).map((x) => initResource(resources[x]));
    return Promise.all(ps);
  }).then(() => done());
});

