const webpack           = require('webpack');
const { resolve }       = require('path');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const args              = require('minimist')(process.argv);

const reduxLogger       = args.rlg;

const config = {
    context: __dirname,
    cache  : true,
    watch  : true,
    devtool: 'source-map',

    entry: './_old/js/src/index.js',

    output: {
        path    : resolve(__dirname, 'js', 'dist'),
        filename: 'bundle.js',
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
                    options: {
                        presets: ['es2015']
                    }
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
        new webpack.DefinePlugin({
            __DEVELOPMENT__    : true,
            __PRODUCTION__     : false,
            __REDUX_LOGGER__   : JSON.parse(reduxLogger || false),
        })
    ]
};

module.exports = config;
