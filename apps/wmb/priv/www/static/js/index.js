import React        from 'react';
import ReactDOM     from 'react-dom';
import { AppContainer } from 'react-hot-loader';
import { Provider } from 'react-redux';
import App                   from './src/components/app';
import configureStore                 from './src/store/configureStore';


const initialState = {};
const store = configureStore(initialState);

const Render = (Component) => {
    if (!__PRODUCTION__) {
        const { showDevTools } = require('./src/devTools.js'); // eslint-disable-line global-require
        ReactDOM.render(
            <AppContainer>
                <Provider store = {store}>
                    <Component/>
                </Provider>
            </AppContainer>
            , document.getElementById('main--container')
        );
    }

    if (__PRODUCTION__) {
        ReactDOM.render(
            <Provider store = {store}>
                <Component/>
            </Provider>
        , document.getElementById('main--container')
        );
    }
};

if (module.hot) {
    module.hot.accept('./src/components/app', () => { Render(App) });
}
