"use strict";

exports._loginStatus = function(fb) {
  return function(onError, onSuccess) {
    fb.getLoginStatus(function(response) {
      onSuccess(response);
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  }
}

exports._login = function(fb) {
  return function(opts) {
    return function(onError, onSuccess) {
      fb.login(function (response) {
        onSuccess(response);
      }, opts);

      return function(cancelError, cancelerError, cancelerSuccess) {
        cancelerSuccess();
      };
    }
  }
}

exports._logout = function(fb) {
  return function(onError, onSuccess) {
    fb.logout(function (response) {
      onSuccess(response);
    });

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  }
}

exports._init = function(fbConfig) {
  return function(onError, onSuccess) {
    window.fbAsyncInit = function() {
      try {
        FB.init(fbConfig);
        FB.AppEvents.logPageView();
        onSuccess(FB);
      }
      catch (err) {
        onError(err.message);
      }
    };
    (function(d, s, id) {
      var js, fjs = d.getElementsByTagName(s)[0];
      if (d.getElementById(id)) return;
      js = d.createElement(s); js.id = id;
      js.src = "//connect.facebook.net/en_US/sdk.js";
      fjs.parentNode.insertBefore(js, fjs);
    }(document, 'script', 'facebook-jssdk'));

    return function(cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess();
    };
  }
}
