const path         = require('path');
const express      = require('express');
const cookieParser = require('cookie-parser');
const http         = require('http');
const https        = require('https');
const webpack      = require('webpack');
const colors       = require('colors');

const httpProxy = require('http-proxy');
const proxy = httpProxy.createProxyServer({
    changeOrigin: true,
    ws          : true,
});

const args = require('minimist')(process.argv);

const port = args['p'] || 3000;
const host = args['h'] || '0.0.0.0';
const devHost = args['devHost'] || 'wmb.diff.org.ua';
const devPort = args['devPort'] ? `:${args['devPort']}` : '';
const remoteProtocol = args['protocol'] || 'http';

function replaceTargetDomain(cookie) {
    const temp = cookie.split(' ');
    temp[temp.length - 1] = `Domain=${host}`;
    //   console.log(`temp:`, temp);
    return temp.join('');
}

const apiHost = `${remoteProtocol}://${devHost}${devPort}`;


const options = {
    host   : devHost,
    potocol: remoteProtocol,
    path   : '/',
};

let PHPSESSID;
let sessionCookie;

const protocol = remoteProtocol === 'http' ? http : https;

protocol.get(options, (res, req) => {
    const cookieIndex = remoteProtocol === 'http' ? 1 : 2;
    sessionCookie = ((res.headers['set-cookie'] || [])[cookieIndex] || '');
    PHPSESSID = ((res.headers['set-cookie'] || [])[cookieIndex] || '').split(';')[0] || '';
    sessionCookie = replaceTargetDomain(sessionCookie);
});

const app = express();
const config = require('./webpack.config.js');
const compiler = webpack(config);


proxy.on('error', (err, req, res) => {
    if (!res.headersSent) {
        res.writeHead(500, { 'content-type': 'application/json' });
    }
    res.end(JSON.stringify({ error: 'proxy_error', reason: err.message }));
});

app.use(cookieParser());

const serverOptions = {
    hot       : true,
    noInfo    : false,
    publicPath: config.output.publicPath,
    stats     : {
        assets      : true,
        colors      : true,
        version     : false,
        hash        : false,
        timings     : true,
        chunks      : false,
        chunkModules: false,
    },
};

app.use(require('webpack-dev-middleware')(compiler, serverOptions));
app.use(require('webpack-hot-middleware')(compiler));

app.use('/dist', express.static('./js/dist'));
app.use('/static/sass/react-bootstrap-switch.css', express.static('./sass/react-bootstrap-switch.css'));

app.use((req, res, next) => {
    if (PHPSESSID && (req.headers.cookie || '').indexOf(PHPSESSID) === -1) {
        req.headers.cookie = '' + PHPSESSID + ';' + req.headers.cookie + ';LANG=ru;';
    }
    res.header('set-cookie', sessionCookie);
    next();
});


/* PAGE HANDLING */
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, './html/new.dev.html'));
});


/* QWERYS */
app.all('*/static/*', (req, res) => {
    proxy.web(req, res, {
        target: apiHost,
    });
});

app.all('*/api/*', (req, res) => {
    proxy.web(req, res, {
        target: apiHost,
    });
});


/* SERVER */
const server = http.createServer(app);

server.on('upgrade', (req, socket, head) => {
    proxy.ws(req, socket, head);
});

server.listen(port, host, (err) => {
    if (err) {
        console.error(err);
        return;
    }
    console.log(colors.green(`Listening at http://${host}:${port}`));
    console.log(colors.blue(`Proxy to ${apiHost}`));
});
