const webpack           = require('webpack');
const { resolve }       = require('path');
const ExtractTextPlugin = require('extract-text-webpack-plugin');


const config = {
    context: __dirname,
    devtool: 'source-map',

    entry: './js/src/index.js',

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
            fetch: 'imports-loader?this=>global!exports-loader?global.fetch!whatwg-fetch'
        }),
        new webpack.optimize.UglifyJsPlugin({
            sourceMap: true,
            compress : {
                warnings: false,
            //   drop_console: true,
            }
        }),
        new ExtractTextPlugin({
            filename: 'style.css'
        })
    ]
};

module.exports = config;
