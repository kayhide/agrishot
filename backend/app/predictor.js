const co = require('co');
const request = require('request');
const promisify = require('util.promisify');

const post_ = promisify(request.post.bind(request));

module.exports = {
  predict(url) {
    return co(function *() {
      const options = {
        uri: `https://southcentralus.api.cognitive.microsoft.com/customvision/v1.0/Prediction/${process.env.CUSTOMVISION_PROJECT_ID}/url`,

        headers: {
          'Content-Type': 'application/json',
          'Prediction-Key': process.env.CUSTOMVISION_PREDICTION_KEY
        },
        json: {
          'url': url
        }
      };
      const res = yield post_(options);
      return res.body.Predictions;
    });
  }
}
