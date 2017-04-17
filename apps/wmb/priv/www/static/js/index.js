import 'babel-polyfill';

import React from 'react';
import { render } from 'react-dom';
import { Provider } from 'react-redux';
import { AppContainer } from 'react-hot-loader';
import App from './src/containers/app';
import store from './src/store/store';

if (__DEVELOPMENT__) { // threre will be 'extract-text-webpack-plugin' in production
    require('../sass/main.scss'); // eslint-disable-line
}

const rootEl = document.getElementById('main--container');

const Render = (Component) => {
    render(
        <AppContainer>
            <Provider store = {store}>
                <Component />
            </Provider>
        </AppContainer>,
        rootEl
    );
};

Render(App);

if (module.hot) module.hot.accept('./src/containers/app', () => Render(App));
