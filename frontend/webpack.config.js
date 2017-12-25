const _ = require('lodash');
const path = require('path');
const webpack = require('webpack');
const CleanupPlugin = require('webpack-cleanup-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlPlugin = require('html-webpack-plugin');
const HtmlExcludeAssetsPlugin = require('html-webpack-exclude-assets-plugin');
const CopyPlugin = require('copy-webpack-plugin');

const OmitPlugin = require('./lib/omit-webpack-plugin');
const helper = require('./lib/helper');

process.env.STAGE = process.env.STAGE || 'dev';
helper.verifyStage(process.env.STAGE);

const output_dir = path.resolve(__dirname,
                                'dist',
                                `${process.env.STAGE}${process.env.DEPLOYING ? "-deploy" : ""}`)

const nameWith = (deploying => {
  if (deploying) {
    return (pre, suf) => `${pre}.[hash]${suf}`;
  } else {
    return (pre, suf) => `${pre}${suf}`;
  };
}) (process.env.DEPLOYING);

const entries = {
  regular: {
    index: './src/entry_general.js',
    admin: './src/entry_admin.js'
  },
  static: {
  }
}


module.exports = {
  entry: {
    ...entries.regular,
    ...entries.static
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

    ...Object.keys(entries.regular).map(k => new HtmlPlugin({
      filename: (k === 'index') ? 'index.html' : `${k}/index.html`,
      template: `./static/${k}.html`,
      chunks: [k]
    })),

    ...Object.keys(entries.static).map(k => new HtmlPlugin({
      filename: `${k}.html`,
      template: `./static/${k}.html`,
      chunks: [k],
      excludeAssets: [/.*\.js/]
    })),

    new CopyPlugin([
      './static/favicon.ico'
    ]),
    new HtmlExcludeAssetsPlugin(),
    new CleanupPlugin(),
    new OmitPlugin({
      chunks: _.keys(entries.static)
    })
  ]
};
