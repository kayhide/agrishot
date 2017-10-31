const AWS = require('aws-sdk');

module.exports = {

  // This is called with the results from from FB.getLoginStatus().
  statusChangeCallback(response) {
    console.log('statusChangeCallback');
    console.log(response);
    // The response object is returned with a status field that lets the
    // app know the current login status of the person.
    // Full docs on the response object can be found in the documentation
    // for FB.getLoginStatus().
    if (response.status === 'connected') {
      this.updateCognito(response.authResponse);
      // Logged into your app and Facebook.
      this.testAPI();
    } else {
      // The person is not logged into your app or we are unable to tell.
      document.getElementById('status').innerHTML = 'Please log ' +
        'into this app.';
    }
  },

  // This function is called when someone finishes with the Login
  // Button.  See the onlogin handler attached to it in the sample
  // code below.
  checkLoginState() {
    const self = this;
    FB.getLoginStatus(function(response) {
      self.statusChangeCallback(response);
    });
  },

  // Here we run a very simple test of the Graph API after login is
  // successful.  See statusChangeCallback() for when this call is made.
  testAPI() {
    console.log('Welcome!  Fetching your information.... ');
    FB.api('/me', function(response) {
      console.log('Successful login for: ' + response.name);
      document.getElementById('status-fb').innerHTML =
        'Thanks for logging in, ' + response.name + '!';
    });
  },


  updateCognito(authResponse) {
    console.log('You are now logged in');
    console.log(JSON.stringify(authResponse));

    AWS.config.update({
      region: 'ap-northeast-1',
      credentials: new AWS.CognitoIdentityCredentials({
        IdentityPoolId: ENV.AWS_IDENTITY_POOL_ID,
        Logins: {
          'graph.facebook.com': authResponse.accessToken
        }
      })
    });
    AWS.config.credentials.get(() => {
      if (AWS.config.credentials.expired) {
        console.log("Failed to authenticate");
        return;
      }
      const db = new AWS.DynamoDB.DocumentClient();
      const params = { TableName: 'agrishot-dev-photos' };
      db.scan(params, function(err, data) {
        console.log('scan is called');
        console.log(params);
        if (err) {
          throw new Error(err);
        }
        else {
          console.log('successed');
          document.getElementById('status-aws').innerHTML =
            'Success to authenticate in AWS<br>' +
            JSON.stringify(data);
          console.log(data);
        }
      });

    });
  }
};
