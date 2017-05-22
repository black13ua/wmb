import { takeEvery, cancel } from 'redux-saga';
import { put, fork, take, call } from 'redux-saga/effects';

// import * as API from '../../api';
import { PLAY_TRACK, STOP_TRACK, TOGGLE_TRACK } from '../../constants/action-types';
import createPlayerChannel from './player-event-emiter';
import { receiveError } from '../../actions';

// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function createPlayerFromUrl(url) {
    return window.AV.Player.fromURL(encodeURI(url));
}

function* generateActionsFromPlayerMessage(playerChannel) {
  while (true) { // eslint-disable-line
      const action = yield take(playerChannel);
      yield put(action);
  }
}

function* watchPlayTrack() {
    while (true) {
        const { payload } = yield take(PLAY_TRACK);
        if (player && player.playing) {
            player.stop();
        }
        const player = yield call(createPlayerFromUrl, payload.track);
        const playerChannel = yield call(createPlayerChannel, player);
        const playerTasks = yield [ // need to be canceled
            fork(generateActionsFromPlayerMessage, playerChannel),
            // fork(readAndWriteToSocket, socket),
            fork(watchStopTrack, player),
            fork(watchToggleTrack, player),
        ];
        player.play();
        yield take(REMOVE_PREVIOUS_PLAYER);
        player.stop();
        yield playerTasks.forEach(task => cancel(task));
    }
}

function* watchStopTrack(player) {
    yield takeEvery(STOP_TRACK);
    try {
        player.stop();
    } catch (error) {
        yield put(receiveError(error.message));
    }
}

function* watchToggleTrack(player) {
    yield takeEvery(TOGGLE_TRACK);
    try {
        player.togglePlayback();
    } catch (error) {
        yield put(receiveError(error.message));
    }
}


export default function* () {
    yield fork(watchPlayTrack);
}
