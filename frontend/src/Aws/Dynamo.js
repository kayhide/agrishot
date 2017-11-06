"use strict";

const AWS = require('aws-sdk');

var client;

exports.setup = function(conf) {
  return function () {
    console.log('setup is called');
    console.log(conf);
    client = new AWS.DynamoDB.DocumentClient(conf);
  };
};

exports._scan = function(params) {
  return function(onSuccess) {
    return function() {
      client.scan(params, function(err, data) {
        console.log('scan is called');
        console.log(params);
        if (err) {
          throw new Error(err);
        }
        else {
          console.log('successed');
          console.log(data);
          onSuccess(data)();
        }
      });
    };
  };
};
