const webpack = require("webpack");
const path = require("path");

module.exports = {
  entry: "./mq.ts",
  target: "web",
  devtool: false,

  output: {
    path: __dirname,
    filename: "bundle.js",
    libraryTarget: "umd",
    globalObject: "this"
  },

  resolve: {
    extensions: [".ts"]
  },

  mode: "development",

  module: {
    rules: [
      {
        test: /\.ts$/,
        use: [
          {
            loader: "ts-loader"
          }
        ]
      }
    ]
  },
  plugins: [new webpack.NamedModulesPlugin()]
};
