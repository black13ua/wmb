const path    = require('path');
const webpack = require('webpack');


const config = {
    cache: true,
    entry: {
        vendor: [
            'react',
            'redux',
            'react-redux',
            'jquery',
            'lodash',
            'moment',
            'classnames',
        ],
    },
    output: {
        path    : path.join(__dirname, 'dist', 'dll'),
        filename: 'dll.[name].js',
        library : '[name]',
    },

    plugins: [
        new webpack.DllPlugin({
            path: path.join(__dirname, 'dist', 'dll', '[name]-manifest.json'),
            name: '[name]',
        }),
        new webpack.optimize.OccurrenceOrderPlugin(),
    ],
};

module.exports = config;
