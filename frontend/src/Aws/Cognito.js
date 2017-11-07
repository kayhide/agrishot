"use strict";

const AWS = require('aws-sdk');

var conf = {};
var params = {
  IdentityPoolId: null,
  Logins: {}
};

exports.setRegion = function(region) {
  return function() {
    conf.region = region;
  };
}

exports.setIdentityPoolId = function(id) {
  return function () {
    params.IdentityPoolId = id;
  };
}

exports.setFacebookToken = function(token) {
  return function () {
    params.Logins['graph.facebook.com'] = token;
  };
}

exports._authenticate = function(onError, onSuccess) {
  console.log(params);
  console.log(conf);
  var credentials = new AWS.CognitoIdentityCredentials(params, conf);
  credentials.get(function(err) {
    if (err) {
      onError(err);
    }
    else {
      onSuccess(
        new AWS.Config({
          region: conf.region,
          credentials: credentials
        })
      );
    }
  });

  return function(cancelError, cancelerError, cancelerSuccess) {
    cancelerSuccess();
  };
};
