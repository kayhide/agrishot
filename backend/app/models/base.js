'use strict';

const co = require('co');
const promisify = require('util.promisify');
const AWS = require('aws-sdk');
const Converter = require('aws-sdk/lib/dynamodb/converter');

const Base = {
  prefix: process.env.RESOURCE_PREFIX,
  region: process.env.AWS_REGION,
  profile: process.env.AWS_PROFILE,
  client: new AWS.DynamoDB.DocumentClient(),

  find(model, id) {
    const params = {
      TableName: `${this.prefix}${model.tableBasename}`,
      Key: { id: id }
    };
    return this.get(params).then(data => new model(data.Item));
  },

  count(model) {
    const params = {
      TableName: `${this.prefix}${model.tableBasename}`,
      Select: 'COUNT'
    };
    return this.scan(params).then(data => data.Count);
  },

  all(model) {
    const params = {
      TableName: `${this.prefix}${model.tableBasename}`
    };
    return this.scan(params).then(data => data.Items.map(model.build));
  },

  save(item) {
    if (item.beforeSave) {
      item.beforeSave();
    }

    const params = {
      TableName: `${this.prefix}${item.constructor.tableBasename}`,
      Item: item.attributes()
    };
    return this.put(params);
  },

  destroy(item) {
    const params = {
      TableName: `${this.prefix}${item.constructor.tableBasename}`,
      Key: { id: item.id }
    };
    return this.delete(params);
  }
};


function setMethods(obj) {
  const client = obj.client
  obj.scan = promisify(client.scan.bind(client));
  obj.get = promisify(client.get.bind(client));
  obj.put = promisify(client.put.bind(client));
  obj.delete = promisify(client.delete.bind(client));
}
setMethods(Base);

module.exports = Base;
