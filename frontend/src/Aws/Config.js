"use strict";

const AWS = require('aws-sdk');

exports.build = function(options) {
  return function() {
    return new AWS.Config(options);
  };
}

exports.readCredentials = function(profile) {
  return function() {
    return new AWS.SharedIniFileCredentials({ profile: profile });
  }
}
