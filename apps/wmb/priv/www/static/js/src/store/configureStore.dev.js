import {
    createStore,
    applyMiddleware,
    compose,
} from 'redux';

import { compact } from 'lodash';

import createSagaMiddleware from 'redux-saga';
import createLogger from 'redux-logger';

// import DevTools from '../containers/DevTools';
import rootReducer from '../reducers';
import SagaManager from '../sagas/sagaManager';

const sagaMiddleware = createSagaMiddleware();

const middleware = compact([
    sagaMiddleware,
    __REDUX_LOGGER__ && createLogger({ collapsed: true, duration: true }),
]);

const enhancer = compose(
    applyMiddleware(...middleware),
    // DevTools.instrument({ maxAge: 15 })
);

function configureStore(initialState = {}) {
    const store = createStore(rootReducer, initialState, enhancer);
    SagaManager.startSagas(sagaMiddleware);
    if (module.hot) {
        // Enable Webpack hot module replacement for reducers
        module.hot.accept('../reducer', () => {
            const nextRootReducer = require('../reducer').default; // eslint-disable-line global-require
            store.replaceReducer(nextRootReducer);
        });
        module.hot.accept('../sagas/SagaManager', () => {
            SagaManager.cancelSagas(store);
            require('../sagas/SagaManager').default.startSagas(sagaMiddleware); // eslint-disable-line global-require
        });
    }
    return store;
}

export default configureStore;
