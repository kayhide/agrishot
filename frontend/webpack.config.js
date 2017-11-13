const _ = require('lodash');
const path = require('path');
const webpack = require('webpack');
const CleanupPlugin = require('webpack-cleanup-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlPlugin = require('html-webpack-plugin');

const helper = require('./lib/helper');

process.env.STAGE = process.env.STAGE || 'dev';
helper.verifyStage(process.env.STAGE);

const output_dir = path.resolve(__dirname, 'dist', process.env.STAGE)

const nameWith = (env => {
  if (env === 'prod') {
    return (pre, suf) => `${pre}.[hash]${suf}`;
  } else {
    return (pre, suf) => `${pre}${suf}`;
  };
}) (process.env.STAGE);


module.exports = {
  entry: {
    admin: './src/entry.js',
    index: './static/static.js',
    privacy_policy: './static/static.js'
  },

  output: {
    filename: nameWith('[name]', '.js'),
    path: output_dir
  },

  module: {
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: 'css-loader'
        })
      },
      {
        test: /\.html$/,
        use: [
          {
            loader: 'html-loader',
            options: {
              interpolate: true
            }
          },
        ]
      },
      {
        test: /\.md$/,
        use: [
          {
            loader: 'html-loader'
          },
          {
            loader: 'markdown-loader'
          }
        ]
      }
    ]
  },

  plugins: [
    new webpack.DefinePlugin({
      STAGE: JSON.stringify(process.env.STAGE),
      ENV: _.mapValues(helper.readPublicEnv(process.env.STAGE), JSON.stringify)
    }),
    new ExtractTextPlugin(nameWith('styles', '.css')),
    new HtmlPlugin({
      filename: 'admin.html',
      template: 'static/admin.html',
      chunks: ['admin']
    }),
    new HtmlPlugin({
      filename: 'index.html',
      template: 'static/index.html',
      chunks: ['index']
    }),
    new HtmlPlugin({
      filename: 'privacy_policy.html',
      template: 'static/privacy_policy.html',
      chunks: ['privacy_policy']
    }),
    new CleanupPlugin()
  ]
};
