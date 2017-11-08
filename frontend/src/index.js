const Agrishot = require('./agrishot.js');

window.Agrishot = Agrishot;

window.fbAsyncInit = function() {
  FB.init({
    appId      : ENV.FACEBOOK_APP_ID,
    xfbml      : true,
    version    : 'v2.1'
  });
  FB.AppEvents.logPageView();
  Agrishot.checkLoginState();
};

(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/en_US/sdk.js";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));
