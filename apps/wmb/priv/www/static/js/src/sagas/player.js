import { put, fork, take } from 'redux-saga/effects';

// import * as API from '../../api';
import { PLAY_TRACK, STOP_TRACK, TOGGLE_TRACK } from '../constants/action-types';
import { receiveError } from '../actions';

let player = null;
// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchPlayTrack() {
    while (true) {
        const { payload } = yield take(PLAY_TRACK);
        if (player && player.playing) {
            player.stop();
        }
        player = window.AV.Player.fromURL(encodeURI(payload.track));
        player.play();
    }
}

function* watchStopTrack() {
    while (true) {
        yield take(STOP_TRACK);
        try {
            player.stop();
        } catch (error) {
            yield put(receiveError(error.message));
        }
    }
}

function* watchToggleTrack() {
    while (true) {
        yield take(TOGGLE_TRACK);
        try {
            player.togglePlayback();
        } catch (error) {
            yield put(receiveError(error.message));
        }
    }
}


export default function* () {
    yield [
        fork(watchPlayTrack),
        fork(watchStopTrack),
        fork(watchToggleTrack),
    ];
}
