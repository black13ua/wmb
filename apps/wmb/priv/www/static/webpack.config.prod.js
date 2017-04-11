const webpack           = require('webpack');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path              = require('path');


const config = {
    context: __dirname,
    devtool: 'source-map',

    entry: {
        main: [
            './js/index.js',
        ],
        vendors: [
            'react',
            'redux',
            'react-redux',
            'jquery',
            'lodash',
            // 'moment',
            'classnames',
        ],
    },

    output: {
        path         : path.resolve(__dirname, 'js', 'dist'),
        publicPath   : '/static/js/dist/',
        filename     : '[name].js',
        chunkFilename: '[id].chunk.js',
        pathinfo     : true,
    },

    resolve: {
        modules   : ['node_modules'],
        extensions: ['.js', '.json', '.jsx', '.scss'],
    },

    module: {
        rules: [
            {
                test   : /\.js$/,
                exclude: /node_modules/,
                use    : {
                    loader: 'babel-loader',
                },
            },
            {
                test: /\.scss/,
                use : ExtractTextPlugin.extract({
                    fallback: 'style-loader',
                    use     : ['css-loader', 'sass-loader'],
                }),
            },
        ],
    },

    plugins: [
        new webpack.ProvidePlugin({
            fetch     : 'imports-loader?this => global!exports-loader?global.fetch!whatwg-fetch',
            _         : 'lodash',
            'window._': 'lodash',
        }),
        new webpack.optimize.UglifyJsPlugin({
            beautify: false,
            comments: false,
            compress: {
                warnings    : false,
                drop_console: false,
            },
        }),
        new webpack.optimize.CommonsChunkPlugin({
            names: ['vendors'],
            // minChunks: 2
        }),
        new ExtractTextPlugin({
            filename: 'style.css',
        }),
        new webpack.DefinePlugin({
            __DEVELOPMENT__: false,
            __PRODUCTION__ : true,
        }),
        new HtmlWebpackPlugin({
            filename: path.join(__dirname, './html/new.html'),
            template: './html/new.template.html',
            inject  : 'body',
            hash    : true,
        }),
    ],
};

module.exports = config;
