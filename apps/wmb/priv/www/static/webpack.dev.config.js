const webpack           = require('webpack');
const HtmlWebpackPlugin = require(`html-webpack-plugin`);
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const path              = require('path');
const args              = require('minimist')(process.argv);

const reduxLogger       = args.rlg;

const config = {
    context: __dirname,
    cache  : true,
    watch  : true,
    devtool: 'source-map',

    entry: {
        main: [
            'babel-polyfill',
            'react-hot-loader/patch',
            './js/index.js',
        ],
        vendor: [
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
        path    : path.resolve(__dirname, 'js', 'dist'),
        publicPath   : `/static/js/dist/`,
        filename     : `[name].js`,
        chunkFilename: `[id].chunk.js`,
        pathinfo     : true
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
                    loader : 'babel-loader',
                    // options: {
                    //     presets: ['es2015']
                    // }
                }
            },
            {
                test: /\.scss/,
                use : ExtractTextPlugin.extract({
                    fallback: 'style-loader',
                    use     : ['css-loader', 'sass-loader']
                })
            },
        ]
    },

    plugins: [
        new webpack.ProvidePlugin({
            fetch     : 'imports-loader?this=>global!exports-loader?global.fetch!whatwg-fetch',
            _         : 'lodash',
            'window._': 'lodash',
        }),
        new ExtractTextPlugin({
            filename: 'style.css'
        }),
        new webpack.optimize.CommonsChunkPlugin({
            names: ['vendor'],
            // minChunks: 2
        }),
        new webpack.DefinePlugin({
            __DEVELOPMENT__    : true,
            __PRODUCTION__     : false,
            __REDUX_LOGGER__   : JSON.parse(reduxLogger || false),
        }),
        new HtmlWebpackPlugin({
            filename: path.join(__dirname, `./html/new.html`),
            template: `./html/new.template.html`,
            inject: `body`,
            hash: true
        }),
    ]
};

module.exports = config;
