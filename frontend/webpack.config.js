const _ = require('lodash');
const path = require('path');
const webpack = require('webpack');
const CleanupPlugin = require('webpack-cleanup-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const StaticSiteGeneratorPlugin = require('static-site-generator-webpack-plugin');

const helper = require('./lib/helper');

process.env.STAGE = process.env.STAGE || 'dev';
helper.verifyStage(process.env.STAGE);

module.exports = {
  entry: {
    admin: './src/entry.js',
    static: './static/entry.js',
    Agrishot: './src/agrishot.js'
  },

  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'dist', process.env.STAGE),
    library: '[name]',
    libraryTarget: 'umd'
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
      ENV: _.mapValues(helper.readPublicEnv(process.env.STAGE), JSON.stringify)
    }),
    new ExtractTextPlugin('styles.css'),
    new StaticSiteGeneratorPlugin({
      entry: 'static'
    }),
    new CleanupPlugin()
  ]
};
