import { takeEvery, takeLatest } from 'redux-saga';
import { put, call, fork, select } from 'redux-saga/effects';

import API from '../../api';
import {
    FETCH_FILTER_DATA,
    FETCH_RANDOM_TRACKS,
    FETCH_ALBUMS_BY_FILTERS,
    FETCH_SEARCH_RESULTS,
} from '../../constants/action-types';
import {
    receiveFilter,
    receiveRandomTracks,
    setFieldValue,
} from '../../actions';


function fetchFilterDataByAlias(alias) {
    switch (alias) {
        case 'abc'   : return API.fetchAbcFilter;
        case 'genres': return API.fetchGenresFilter;
        case 'dates' : return API.fetchDatesFilter;
    }
}

// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineInitFilter(action) {
    const { alias } = action.payload;
    const response = yield call(fetchFilterDataByAlias(alias));
    yield put(receiveFilter(alias, response));
}

function* routineDataByFilter(action) {
    const { alias, value } = action.payload;
    yield put(setFieldValue(alias, value));
    const currentFilters = yield select(state => state.songs.viewState.filtersCurrentValue.asMutable());
    const response = yield call(API.fetchDataByFilters.bind(null, currentFilters));
    console.log('%c dataByFilters', 'color: aqua', response);
    // yield put(receiveData(response));
}

function* routineRandomButton() {
    const response = yield call(API.fetchRandom);
    yield put(receiveRandomTracks(response));
}

function* routineSearchResults() {
    const search = yield select(state => state.songs.viewState.search);
    const response = yield call(API.fetchDataBySearch.bind(null, search));
    console.log('%c dataBySearch', 'color: aqua', response);
    // yield put(receiveData(response));
}


// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchFilterInitData() {
    yield takeEvery(FETCH_FILTER_DATA, routineInitFilter);
}

function* watchFiltersChanged() {
    yield takeEvery(FETCH_ALBUMS_BY_FILTERS, routineDataByFilter);
}

function* watchRandomButton() {
    yield takeEvery(FETCH_RANDOM_TRACKS, routineRandomButton);
}

function* watchSearch() {
    yield takeLatest(FETCH_SEARCH_RESULTS, routineSearchResults);
}


export default function* () {
    yield [
        fork(watchFilterInitData),
        fork(watchFiltersChanged),
        fork(watchRandomButton),
        fork(watchSearch),
    ];
}
