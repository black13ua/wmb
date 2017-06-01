import { takeEvery, takeLatest } from 'redux-saga';
import { put, call, fork, select } from 'redux-saga/effects';

import * as API from '../../api';
import {
    FETCH_FILTER_DATA,
    FETCH_RANDOM_TRACKS,
    FETCH_ALBUMS_BY_FILTERS,
    FETCH_SEARCH_RESULTS,
    FETCH_ARTISTS_BY_LETTER,
    FETCH_ALBUMS_BY_ARTIST,
} from '../../constants/action-types';
import {
    // receiveFilters,
    receiveFilter,
    receiveRandomTracks,
    setFieldValue,
    receiveError,
    setActiveArtistInAbcFilter,
    receiveArtistsByLetter,
    receiveAlbums,
    saveSearchValue,
} from '../../actions';

import { getRandomNumber, getArtistsByLetter, getSearchValue } from '../../selectors';

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
    return API[apiMethod]
        ? API[apiMethod](...args)
            .then(payload => ({ payload }))
            .catch(error => ({ error: error.message }))
        : new Promise((resolve, reject) => reject());
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
    const randomNumber = yield select(getRandomNumber);
    const { payload, error } = yield call(getDataFromApi, 'fetchRandom', [randomNumber]);
    if (payload) {
        yield put(receiveRandomTracks(payload));
    } else {
        yield put(receiveError(error));
    }
}

function* routineInitFilter(action) {
    const { alias } = action.payload;
    const { payload, error } = yield call(getDataFromApi, filtersApiMethods[alias]);
    if (payload) {
        yield put(receiveFilter(alias, payload));
    } else {
        yield put(receiveError(error));
    }
}

function* routineSearchResults() {
    const search = yield select(getSearchValue);
    const { payload, error } = yield call(getDataFromApi, 'fetchDataBySearch', [search]);
    if (payload) {
        if (_.isEmpty(payload)) {
            yield put(receiveError('Nothing was found'));
        } else {
            yield put(receiveAlbums(payload));
            yield put(saveSearchValue(''));
        }
    } else {
        yield put(receiveError(error));
    }
}

function* routineAbcFilterLetters(action) {
    const { letterId } = action.payload;
    const artistsByLetter = yield select(getArtistsByLetter, letterId);
    if (!artistsByLetter) {
        const { payload, error } = yield call(getDataFromApi, 'fetchArtistsByLetter', [letterId]);
        if (payload) {
            yield put(receiveArtistsByLetter(letterId, payload));
        } else {
            yield put(receiveError(error));
        }
    }
}

function* routineAbcFilterArtist(action) {
    const { artistId } = action.payload;
    const { payload, error } = yield call(getDataFromApi, 'fetchAlbumsByArtist', [artistId]);
    if (payload) {
        yield put(setActiveArtistInAbcFilter(artistId));
        yield put(receiveAlbums(payload));
    } else {
        yield put(receiveError(error));
    }
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

function* watchAbcFilterLetters() {
    yield takeLatest(FETCH_ARTISTS_BY_LETTER, routineAbcFilterLetters);
}

function* watchAbcFilterArtist() {
    yield takeLatest(FETCH_ALBUMS_BY_ARTIST, routineAbcFilterArtist);
}


export default function* () {
    yield [
        // fork(watchFiltersInitData),
        fork(watchFilterInitData),
        fork(watchFiltersChanged),
        fork(watchRandomButton),
        fork(watchSearch),
        fork(watchAbcFilterLetters),
        fork(watchAbcFilterArtist),
    ];
}
