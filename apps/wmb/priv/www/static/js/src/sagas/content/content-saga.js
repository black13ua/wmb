import { put, call, fork, take } from 'redux-saga/effects';

import * as API from '../../api';
import { FETCH_ALBUMS_BY_PAGE } from '../../constants/action-types';
import { receiveAlbums, setCurrentPage, receiveError, fetching } from '../../actions';


function getAlbumsByPage(currentPage) {
    return API.fetchAlbumsByPage(currentPage)
        .then(payload => ({ payload }))
        .catch(error => ({ error: error.message }));
}
// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineDataByPage(currentPage) {
    const { payload, error } = yield call(getAlbumsByPage, currentPage);
    if (payload) {
        yield put(receiveAlbums(payload));
        yield put(setCurrentPage(currentPage));
    } else {
        yield put(receiveError(error));
    }
    yield put(fetching('albums', false));
}

// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchAlbumsByPage() {
    while (true) {
        const { payload } = yield take(FETCH_ALBUMS_BY_PAGE);
        yield put(fetching('albums', true));
        yield fork(routineDataByPage, payload.currentPage);
    }
}


export default function* () {
    yield [
        fork(watchAlbumsByPage),
    ];
}
