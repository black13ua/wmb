import { createStore, applyMiddleware, compose } from 'redux';
// import { syncHistory } from 'react-router-redux';
// import { syncHistory } from 'redux-router-immutable';
import createSagaMiddleware from 'redux-saga';
import thunk from 'redux-thunk';
import promiseMiddleware from '../../middleware/promiseMiddleware';
import logger from '../logger';
import DevTools from '../../containers/common/DevTools';
// import history from './history';
import rootReducer from '../../reducers/live';
import basketSaga from '../../sagas/common/basketSaga';
import startingServiceMenu from '../../sagas/common/service-menu';
import liveSaga from '../../sagas/live/live';
import hashSaga from '../../sagas/common/hashSaga';
// import socketSaga from '../../sagas/common/sockets';
import jackpotWinnerSaga from '../../sagas/common/jackpotWinner';

// const reduxRouter = syncHistory(history);
const sagaMiddleware = createSagaMiddleware();
export default function configureStore(initialState) {
  let createStoreWithMiddleware;

  if (__DEV__) {
    createStoreWithMiddleware = compose(
      applyMiddleware(
        // reduxRouter,
        // sagaMiddleware(basketSaga),
        sagaMiddleware,
        promiseMiddleware,
        thunk,
        logger,
      ),
      DevTools.instrument(),
    )(createStore);
  } else {
    createStoreWithMiddleware = compose(
      applyMiddleware(
        // reduxRouter,
        sagaMiddleware,
        // sagaMiddleware(basketSaga),
        promiseMiddleware,
        thunk,
      ),
    )(createStore);
  }

  const store = createStoreWithMiddleware(rootReducer, initialState);
  sagaMiddleware.run(basketSaga);
  sagaMiddleware.run(startingServiceMenu);
  sagaMiddleware.run(liveSaga);
  sagaMiddleware.run(hashSaga);
  // sagaMiddleware.run(socketSaga);
  sagaMiddleware.run(jackpotWinnerSaga);
  if (module.hot) {
    // Enable Webpack hot module replacement for reducers
    module.hot.accept('../../reducers/live', () => {
      const nextRootReducer = require('../../reducers/live');
      store.replaceReducer(nextRootReducer);
    });
  }

  // Required for replaying actions from devtools to work
  // reduxRouter.listenForReplays(store);

  return store;
}
