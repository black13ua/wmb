const webpack           = require('webpack');
// const ExtractTextPlugin = require('extract-text-webpack-plugin');
const path              = require('path');
const args              = require('minimist')(process.argv);

const reduxLogger       = args.rlg;


const config = {
    context: __dirname,
    cache  : true,
    devtool: 'source-map',

    entry: {
        main: [
            'webpack-hot-middleware/client',
            'react-hot-loader/patch',
            './js/index.js',
        ],
    },

    output: {
        path         : path.resolve(__dirname, 'js', 'dist'),
        publicPath   : '/assets/',
        filename     : '[name].js',
        chunkFilename: '[id].chunk.js',
        pathinfo     : true,
    },

    resolve: {
        modules   : ['node_modules'],
        extensions: ['.js', '.json', '.jsx', '.scss', '.css'],
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
                use : ['style-loader', 'css-loader', 'sass-loader'],
            },
            {
                test: /\.css$/,
                use : [
                    'style-loader',
                    {
                        loader : 'css-loader',
                        options: {
                            modules       : true,
                            sourceMap     : true,
                            importLoaders : 1,
                            localIdentName: '[name]--[local]--[hash:base64:8]',
                        },
                    },
                    'postcss-loader', // has separate config, see postcss.config.js nearby
                ],
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                use : [
                    {
                        loader : 'url-loader',
                        options: {
                            limit   : 10000,
                            mimetype: 'application/font-woff',
                        },
                    },
                ],
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                use : [
                    {
                        loader: 'file-loader',
                    },
                ],
            },
        ],
    },

    plugins: [
        new webpack.DllReferencePlugin({
            context : '.',
            manifest: require('./js/dist/dll/vendors-manifest.json'), // eslint-disable-line
        }),
        new webpack.HotModuleReplacementPlugin(),
        new webpack.ProvidePlugin({
            fetch     : 'imports-loader?this=>global!exports-loader?global.fetch!whatwg-fetch',
            _         : 'lodash',
            'window._': 'lodash',
        }),
        // new ExtractTextPlugin({
        //     filename: 'style.css',
        // }),
        new webpack.LoaderOptionsPlugin({
            debug: true,
        }),
        new webpack.NamedModulesPlugin(),
        new webpack.DefinePlugin({
            __DEVELOPMENT__ : true,
            __PRODUCTION__  : false,
            __REDUX_LOGGER__: JSON.parse(reduxLogger || false),
        }),
    ],
};

module.exports = config;
