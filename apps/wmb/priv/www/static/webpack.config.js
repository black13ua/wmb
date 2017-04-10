const chalk = require('chalk');
const args = require('minimist')(process.argv);

const devTools = JSON.parse(args['d'] || false);
const isForLog = JSON.parse(args['l'] || false);
process.env.NODE_ENV = process.env.NODE_ENV === 'production' ? process.env.NODE_ENV : 'development';

const config = process.env.NODE_ENV === 'production'
    ? require('./webpack.config.prod.js')
    : require('./webpack.config.dev.js');

const nodeEnv  = `    NODE_ENV: ${
     process.env.NODE_ENV === 'production'
        ? chalk.bold.red(process.env.NODE_ENV)
        : chalk.bold.green(process.env.NODE_ENV)}`;

const dt = `__DEVTOOLS__: ${devTools}`;

if (!isForLog) {
    console.log(chalk.bold.green(nodeEnv));
    console.log(chalk.bold.blue(dt));
}

module.exports = config;
