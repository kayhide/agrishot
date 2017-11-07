"use strict";

const AWS = require('aws-sdk');

var client;

exports.setup = function(conf) {
  return function () {
    client = new AWS.DynamoDB.DocumentClient(conf);
  };
};

exports._scan = function(params) {
  return function(onError, onSuccess) {
    client.scan(params, function(err, data) {
      console.log(params);
      if (err) {
        onError(err);
      }
      else {
        onSuccess(data);
      }
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};
