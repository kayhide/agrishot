"use strict";

const AWS = require('aws-sdk');

let client;

exports._setup = function(conf) {
  client = new AWS.DynamoDB.DocumentClient(conf);
};

exports._scan = function() {
  return function(params) {
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
};
