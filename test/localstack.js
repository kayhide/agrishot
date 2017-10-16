'use strict';

const AWS = require('aws-sdk');

function localize(base, origin, extras = {}) {
  return class {
    constructor(options = {}) {
      Object.assign(options, {region: 'local', endpoint: `http://${origin}`});
      Object.assign(options, extras);
      return new base(options);
    }
  };
}

module.exports.DynamoDB = localize(AWS.DynamoDB, 'localhost:4569');
module.exports.DynamoDB.DocumentClient = localize(AWS.DynamoDB.DocumentClient, 'localhost:4569');
module.exports.S3 = localize(AWS.S3, 'localhost:4572', {s3ForcePathStyle: true});
