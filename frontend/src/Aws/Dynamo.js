"use strict";

const AWS = require('aws-sdk');

var client;

exports.setup = function(conf) {
  return function () {
    client = new AWS.DynamoDB.DocumentClient(conf);
  };
};

var verbose = false;

exports.verbose = function(b) {
  return function () {
    verbose = b;
  };
};

exports._scan = function(params) {
  return function(onError, onSuccess) {
    client.scan(params, function(err, data) {
      if (verbose) {
        console.log(params);
        console.log(data);
      }
      err ? onError(err) : onSuccess(data)
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

exports._query = function(params) {
  return function(onError, onSuccess) {
    client.query(params, function(err, data) {
      if (verbose) {
        console.log(params);
        console.log(data);
      }
      err ? onError(err) : onSuccess(data)
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

exports._get = function(params) {
  return function(onError, onSuccess) {
    client.get(params, function(err, data) {
      if (verbose) {
        console.log(params);
        console.log(data);
      }
      err ? onError(err) : onSuccess(data)
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

exports._put = function(params) {
  return function(onError, onSuccess) {
    client.put(params, function(err, data) {
      if (verbose) {
        console.log(params);
        console.log(data);
      }
      err ? onError(err) : onSuccess(data)
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

exports._delete = function(params) {
  return function(onError, onSuccess) {
    client.delete(params, function(err, data) {
      if (verbose) {
        console.log(params);
        console.log(data);
      }
      err ? onError(err) : onSuccess(data)
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};

exports._count = function(tableName) {
  return function(onError, onSuccess) {
    var params = {
      "TableName": tableName,
      "Select": 'COUNT'
    };
    client.scan(params, function(err, data) {
      if (verbose) {
        console.log(params);
        console.log(data);
      }
      err ? onError(err) : onSuccess(data.Count)
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  };
};
