import { takeLatest } from 'redux-saga';
import { put, call, fork } from 'redux-saga/effects';

import API from '../../api';
import { FETCH_ABC_FILTER } from '../../constants/action-types';
import { receiveABCfilter, fetching } from '../../actions';


// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineABCFilter() {
    yield put(fetching('abc'));
    const response = yield call(API.fetchFilterABC);
    yield put(fetching('abc'));
    yield put(receiveABCfilter(response));
}


// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchABCfilter() {
    yield takeLatest(FETCH_ABC_FILTER, routineABCFilter);
}

// function* watchSearch() {
//     yield takeLatest(FETCH_SEARCH_RESULTS, routineSearchResults);
// }


export default function* () {
    yield [
        fork(watchABCfilter),
        // fork(watchSearch),
    ];
}
