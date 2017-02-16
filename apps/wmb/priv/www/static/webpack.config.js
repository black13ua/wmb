const webpack = require('webpack');
//const path = require('path');

const config = {
    context: __dirname,
    cache: true,
    watch: true,

    entry: {
        main: [
            './js/main.js'
        ]
    },

    output: {
        path    : '../../compressed/',
        filename: 'hls-player.js',
        pathinfo: true
    },

    resolveLoader: {
        modulesDirectories: ['node_modules']
    },

    module: {
        loaders: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                loader: 'babel',
                query: {
                    presets: ['react', 'es2015']
                }
            },
            { test: /\.scss/, loader: `style!css-loader!sass` },
            { test: /\.css$/, loader: 'style!css' }
        ]
    },

    plugins: [
        new webpack.ProvidePlugin({
            fetch: 'imports?this=>global!exports?global.fetch!whatwg-fetch'
        })
    ]
};

module.exports = config;
