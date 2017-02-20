const webpack = require('webpack');
const { resolve } = require('path');

const config = {
    context: __dirname,

    entry: './js/src/index.js',

    output: {
        path    : resolve(__dirname, 'js', 'dist'),
        filename: 'bundle.js',
    },

    resolve: {
        modules: ['node_modules'],
        extensions: [".js", ".json", ".jsx", ".scss"],
    },

    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: {
                        presets: ['es2015']
                    }
                }
            },
            { test: /\.scss/, use: ['style-loader', 'css-loader', 'sass-loader'] },
            { test: /\.css$/, use: ['style-loader', 'css-loader'] }
        ]
    },

    plugins: [
        new webpack.ProvidePlugin({
            fetch: 'imports?this=>global!exports?global.fetch!whatwg-fetch'
        }),
        new webpack.optimize.UglifyJsPlugin({
          sourceMap: true,
          compress: {
              warnings: false,
              drop_console: true,
          }
        })
    ]
};

module.exports = config;
