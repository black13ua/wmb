import { takeEvery, fork } from 'redux-saga/effects';
import { keys } from 'lodash';

export default function (routines) {
    const actionTypes = keys(routines);
    return function* watcher() {
        yield takeEvery(actionTypes, function* routineHandler(action) {
            yield fork(routines[action.type], action);
        });
    };
}
