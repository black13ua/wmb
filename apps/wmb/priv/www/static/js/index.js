import 'babel-polyfill';
import React        from 'react';
import ReactDOM     from 'react-dom';
import { AppContainer } from 'react-hot-loader';
import { Provider } from 'react-redux';
import App                   from './src/components/app';
import configureStore                 from './src/store/configureStore';


const initialState = {};
const store = configureStore(initialState);

const Render = () => {
    if (!__PRODUCTION__) {
        const { showDevTools } = require('./src/devTools.js'); // eslint-disable-line global-require
        ReactDOM.render(
            <AppContainer>
                <Provider store = {store}>
                    <App/>
                </Provider>
            </AppContainer>
            , document.getElementById('root')
        );
    }

    if (__PRODUCTION__) {
        ReactDOM.render(
            <AppContainer>
                <Provider store = {store}>
                    <App/>
                </Provider>
            </AppContainer>
            , document.getElementById('root')
        );
    }
};

if (module.hot) {
    module.hot.accept('./src/components/app', () => {
        // If you use Webpack 2 in ES modules mode, you can
        // use <App /> here rather than require() a <NextApp />.
        const NextApp = require('./src/components/app').default;
        ReactDOM.render(
            <AppContainer>
                <Provider store = {store}>
                    <NextApp />
                </Provider>
            </AppContainer>,
            document.getElementById('root')
        );
    });
}
