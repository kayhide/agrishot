"use strict";

const AWS = require('aws-sdk');

exports.build = function(options) {
  return function() {
    return new AWS.Config(options);
  };
}
