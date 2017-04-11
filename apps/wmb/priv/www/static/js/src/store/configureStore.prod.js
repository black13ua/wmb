import { createStore, applyMiddleware } from 'redux';
import createSagaMiddleware from 'redux-saga';

import rootReducer from '../reducers';
import SagaManager from '../sagas/sagaManager';

export default function configureStore(initialState = {}) {
    // create saga middleware
    const sagaMiddleware = createSagaMiddleware();

    // create middleware function
    const middleware = applyMiddleware(sagaMiddleware);
    const store = middleware(createStore)(rootReducer, initialState);

    // run sagas
    SagaManager.startSagas(sagaMiddleware);

    return store;
}
