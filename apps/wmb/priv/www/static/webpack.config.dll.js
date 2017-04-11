const path    = require('path');
const webpack = require('webpack');

const config = {
    context: __dirname,
    cache  : true,
    entry  : {
        vendors: [path.join(__dirname, 'vendors-src.js')],
    },
    output: {
        path    : path.join(__dirname, 'js', 'dist', 'dll'),
        filename: 'dll.[name].js',
        library : '[name]',
    },
    module: {
        loaders: [
            { test: /\.json$/, loader: 'json-loader' },
        ],
    },

    plugins: [
        new webpack.DllPlugin({
            path   : path.join(__dirname, 'js', 'dist', 'dll', '[name]-manifest.json'),
            name   : '[name]',
            context: __dirname,
        }),
        new webpack.optimize.OccurrenceOrderPlugin(),
    ],
};

module.exports = config;
