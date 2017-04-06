import { createStore, applyMiddleware, compose } from 'redux';
// import { syncHistory } from 'react-router-redux';
import createSagaMiddleware from 'redux-saga';
import promiseMiddleware from '../../middleware/promiseMiddleware';
import rootReducer from '../../reducers/live';
import apiSaga from '../../sagas/live/live';
import IOSaga from '../../sagas/live/hash';
// import socketSaga from '../../sagas/common/sockets';

// const reduxRouter = syncHistory(history);
const sagaMiddleware = createSagaMiddleware();
export default function configureStore(initialState) {
    const createStoreWithMiddleware = compose(
      applyMiddleware(
        sagaMiddleware,
        promiseMiddleware,
      ),
    )(createStore);

    const store = createStoreWithMiddleware(rootReducer, initialState);
    sagaMiddleware.run(apiSaga);
    sagaMiddleware.run(IOSaga);
    if (module.hot) {
    // Enable Webpack hot module replacement for reducers
        module.hot.accept('../../reducers/live', () => {
            const nextRootReducer = require('../../reducers/live');
            store.replaceReducer(nextRootReducer);
        });
    }
    return store;
}
