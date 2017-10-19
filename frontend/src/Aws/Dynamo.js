"use strict";

const AWS = require('aws-sdk');

exports._scan = function(conf) {
  console.log(conf);
  const dynamo = new AWS.DynamoDB.DocumentClient(conf);
  return function(params) {
    return function(onSuccess) {
      return function() {
        dynamo.scan(params, function(err, data) {
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
