import { takeLatest } from 'redux-saga';
import { put, call, fork } from 'redux-saga/effects';

import API from '../../api';
import { FETCH_ALBUMS_BY_FILTERS } from '../../constants/action-types';
import { receiveAlbums } from '../../actions';


// ******************************************************************************/
// ******************************* ROUTINES *************************************/
// ******************************************************************************/

function* routineDataByPage(action) {
    const { page } = action.payload;
    const response = yield call(API.fetchAlbumsByPage.bind(null, page));
    yield put(receiveAlbums(response));
}

// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchDataByPage() {
    yield takeLatest(FETCH_ALBUMS_BY_FILTERS, routineDataByPage);
}


export default function* () {
    yield [
        fork(watchDataByPage),
    ];
}
