import { put, call, fork, take } from 'redux-saga/effects';

import * as API from '../../api';
import { FETCH_ALBUMS_BY_PAGE } from '../../constants/action-types';
import { receiveAlbums, setCurrentPage } from '../../actions';

// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineDataByPage(currentPage) {
    const response = yield call(API.fetchAlbumsByPage.bind(null, currentPage));
    yield put(receiveAlbums(response));
}

// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchAlbumsByPage() {
    while (true) {
        const { payload } = yield take(FETCH_ALBUMS_BY_PAGE);
        yield put(setCurrentPage(payload.currentPage));
        yield fork(routineDataByPage, payload.currentPage);
    }
}


export default function* () {
    yield [
        fork(watchAlbumsByPage),
    ];
}
