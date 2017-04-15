import { takeLatest, takeEvery } from 'redux-saga';
import { put, call, fork } from 'redux-saga/effects';

import API from '../../api';
import {
    FETCH_ABC_FILTER,
    FETCH_GENRES_FILTER,
    FETCH_DATES_FILTER,
    FETCH_RANDOM_TRACKS,
} from '../../constants/action-types';
import {
    receiveABCfilter,
    receiveGenresFilter,
    receiveDatesFilter,
    receiveRandomTracks,
} from '../../actions';


// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineInitABCFilter() {
    const response = yield call(API.fetchFilterABC);
    yield put(receiveABCfilter(response));
}
function* routineInitGenresFilter() {
    const response = yield call(API.fetchGenresFilter);
    yield put(receiveGenresFilter(response));
}
function* routineInitDatesFilter() {
    const response = yield call(API.fetchDatesFilter);
    yield put(receiveDatesFilter(response));
}

function* routineRandomButton() {
    const response = yield call(API.fetchRandom);
    yield put(receiveRandomTracks(response));
}


// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchInitABCfilter() {
    yield takeLatest(FETCH_ABC_FILTER, routineInitABCFilter);
}

function* watchInitGenresFilter() {
    yield takeLatest(FETCH_GENRES_FILTER, routineInitGenresFilter);
}

function* watchInitDatesFilter() {
    yield takeLatest(FETCH_DATES_FILTER, routineInitDatesFilter);
}

function* watchRandomButton() {
    yield takeEvery(FETCH_RANDOM_TRACKS, routineRandomButton);
}

// function* watchSearch() {
//     yield takeLatest(FETCH_SEARCH_RESULTS, routineSearchResults);
// }


export default function* () {
    yield [
        fork(watchInitABCfilter),
        fork(watchInitGenresFilter),
        fork(watchInitDatesFilter),
        fork(watchRandomButton),
        // fork(watchSearch),
    ];
}
