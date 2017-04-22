import { takeEvery, takeLatest } from 'redux-saga';
import { put, call, fork, select } from 'redux-saga/effects';

import * as API from '../../api';
import {
    FETCH_FILTER_DATA,
    FETCH_RANDOM_TRACKS,
    FETCH_ALBUMS_BY_FILTERS,
    FETCH_SEARCH_RESULTS,
    FETCH_ARTISTS_BY_LETTER,
} from '../../constants/action-types';
import {
    // receiveFilters,
    receiveFilter,
    receiveRandomTracks,
    setFieldValue,
    receiveError,
} from '../../actions';


const filtersApiMethods = {
    abc   : 'fetchAbcFilter',
    genres: 'fetchGenresFilter',
    dates : 'fetchDatesFilter',
};

// function fetchFiltersData(keys) {
//     return Promise.all(keys.map(key => filtersApiMethods[key]))
//         .then(payloads => ({ payload: {
//             abc   : payloads[0],
//             genres: payloads[1],
//             dates : payloads[2],
//         } }))
//         .catch(error => ({ error }));
// }

function getDataFromApi(apiMethod, args = []) {
    return API[apiMethod](...args)
        .then(payload => ({ payload }))
        .catch(error => ({ error }));
}

// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

// function* routineInitFilters() {
//     const filtersKeys = yield select(state => _.keys(state.filters.data.filters.asMutable()));
//     const { payload, error } = yield call(fetchFiltersData, filtersKeys);
//     if (payload) {
//         yield put(receiveFilters(payload));
//     } else {
//         yield put(receiveError(error));
//     }
// }

function* routineDataByFilter(action) {
    const { alias, value } = action.payload;
    yield put(setFieldValue(alias, value));
    const currentFilters = yield select(state => state.filters.viewState.filtersCurrentValue.asMutable());
    const { payload, error } = yield call(getDataFromApi, 'fetchDataByFilters', [currentFilters]);
    if (payload) {
        console.log('%c dataByFilters', 'color: aqua', payload);
        // yield put(receiveData(payload));
    } else {
        yield put(receiveError(error));
    }
}

function* routineRandomButton() {
    const { payload, error } = yield call(getDataFromApi, 'fetchRandom');
    if (payload) {
        yield put(receiveRandomTracks(payload));
    } else {
        yield put(receiveError(error));
    }
}

function* routineInitFilter(action) {
    const { alias } = action.payload;
    console.info('filtersApiMethods[alias]', filtersApiMethods[alias]);
    const { payload, error } = yield call(getDataFromApi, filtersApiMethods[alias]);
    if (payload) {
        yield put(receiveFilter(alias, payload));
    } else {
        yield put(receiveError(error));
    }
}

function* routineSearchResults() {
    const search = yield select(state => state.filters.viewState.search);
    const { payload, error } = yield call(getDataFromApi, 'fetchDataBySearch', [search]);
    if (payload) {
        console.log('%c dataBySearch', 'color: aqua', payload);
        // yield put(receiveData(payload));
    } else {
        yield put(receiveError(error));
    }
}

function* routineAbcFilterLetters(action) {
    const { letterId } = action;
    const artistsByLetter = yield select(state => state.filters.artistsByLetter[letterId]);
    if (!artistsByLetter) {
        const { payload, error } = yield call(getDataFromApi, 'fetchArtistByLetter', [letterId]);
        if (payload) {
            console.log('%c dataBySearch', 'color: aqua', payload);
            // yield put(receiveData(payload));
        } else {
            yield put(receiveError(error));
        }
    }
}


// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

// function* watchFiltersInitData() {
//     yield takeEvery(FETCH_FILTERS_DATA, routineInitFilters);
// }

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

function* watchAbcFilterLetters() {
    yield takeLatest(FETCH_ARTISTS_BY_LETTER, routineAbcFilterLetters);
}


export default function* () {
    yield [
        // fork(watchFiltersInitData),
        fork(watchFilterInitData),
        fork(watchFiltersChanged),
        fork(watchRandomButton),
        fork(watchSearch),
        fork(watchAbcFilterLetters),
    ];
}
