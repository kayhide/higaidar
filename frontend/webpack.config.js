const _ = require('lodash');
const path = require('path');
const webpack = require('webpack');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
// const CleanupPlugin = require('webpack-cleanup-plugin');
// const ExtractTextPlugin = require('extract-text-webpack-plugin');
// const HtmlPlugin = require('html-webpack-plugin');
// const HtmlExcludeAssetsPlugin = require('html-webpack-exclude-assets-plugin');
// const CopyPlugin = require('copy-webpack-plugin');

// const OmitPlugin = require('./lib/omit-webpack-plugin');
const helper = require('./lib/helper');

process.env.STAGE = process.env.STAGE || 'dev';
helper.verifyStage(process.env.STAGE);

const output_dir = path.resolve(
  __dirname,
  'dist',
  `${process.env.STAGE}${process.env.DEPLOYING ? "-deploy" : ""}`
);

const isDevServer = process.argv.some(a => path.basename(a) === "webpack-dev-server");
const isWatch = process.argv.some(a => a === "--watch");

const publicPath = "/";

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

  plugins: [
    new webpack.DefinePlugin({
      STAGE: JSON.stringify(process.env.STAGE),
      ENV: _.mapValues(helper.readPublicEnv(process.env.STAGE), JSON.stringify)
    }),
    new MiniCssExtractPlugin({
      filename: `css/[name]${isDevServer ? "" : "-[contenthash:8]"}.css`,
      chunkFilename: `css/[name]${isDevServer ? "" : "-[contenthash:8]"}.chunk.css`,
      publicPath,
      ignoreOrder: false
    }),
    // new ExtractTextPlugin(nameWith('styles', '.css')),

    // ...Object.keys(entries.regular).map(k => new HtmlPlugin({
    //   filename: (k === 'index') ? 'index.html' : `${k}/index.html`,
    //   template: `./static/${k}.html`,
    //   chunks: [k]
    // })),

    // ...Object.keys(entries.static).map(k => new HtmlPlugin({
    //   filename: `${k}.html`,
    //   template: `./static/${k}.html`,
    //   chunks: [k],
    //   excludeAssets: [/.*\.js/]
    // })),

    // new CopyPlugin([
    //   './static/favicon.ico'
    // ]),
    // new HtmlExcludeAssetsPlugin(),
    // new CleanupPlugin(),
    // new OmitPlugin({
    //   chunks: _.keys(entries.static)
    // })
  ],

  module: {
    rules: [
      {
        test: /\.css$/,
        use:
        [
          { loader: MiniCssExtractPlugin.loader },
          {
            loader: "css-loader",
            options: {
              sourceMap: true
            }
          }
        ]
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
      },
      {
        test: /\.purs$/,
        use: [
          {
            loader: "purs-loader",
            options: {
              spago: true,
              src: [],
              watch: isDevServer || isWatch,
            }
          },
        ]
      },
    ]
  },

  devServer: {
    host: "0.0.0.0",
    port: "3000",
    disableHostCheck: true,
    hot: true,
    headers: {
      "Access-Control-Allow-Origin": "*"
    },
    publicPath
  }
};
