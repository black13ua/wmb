import { take, fork, cancel } from 'redux-saga/effects';
import { map } from 'lodash';
import rootSaga from './index';

export const CANCEL_SAGAS_HMR = 'CANCEL_SAGAS_HMR';

function createAbortableSaga(saga) {
    if (__DEVELOPMENT__) {
        return function* main() {
            const sagaTask = yield fork(saga);

            yield take(CANCEL_SAGAS_HMR);
            yield cancel(sagaTask);
        };
    }
    return saga;
}

const SagaManager = {
    startSagas(sagaMiddleware) {
        map(rootSaga, createAbortableSaga)
            .forEach(saga => sagaMiddleware.run(saga));
    },

    cancelSagas(store) {
        store.dispatch({
            type: CANCEL_SAGAS_HMR,
        });
    },
};

export default SagaManager;
