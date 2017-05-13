import { takeLatest } from 'redux-saga';
import { put, call, fork, select } from 'redux-saga/effects';

import * as API from '../../api';
import { FETCH_ALBUMS_BY_PAGE } from '../../constants/action-types';
import { receiveAlbums } from '../../actions';


// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineDataByPage() {
    const page = yield select(state => state.music.viewState.currentPage);
    const response = yield call(API.fetchAlbumsByPage.bind(null, page));
    yield put(receiveAlbums(response));
}

// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchAlbumsByPage() {
    yield takeLatest(FETCH_ALBUMS_BY_PAGE, routineDataByPage);
}


export default function* () {
    yield [
        fork(watchAlbumsByPage),
    ];
}
