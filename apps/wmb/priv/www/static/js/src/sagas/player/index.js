import { put, fork, take, call, cancel } from 'redux-saga/effects';

// import * as API from '../../api';
import {
    PLAY_TRACK,
    STOP_TRACK,
    TOGGLE_TRACK,
    ASK_PLAYER_PROPERTY,
    SET_PLAYER_PROPERTY,
    REMOVE_PREVIOUS_PLAYER,
} from '../../constants/action-types';
import createPlayerChannel from './player-event-emiter';
import { receiveError, getPlayerProperty } from '../../actions';

// ******************************************************************************/
// ******************************* WATCHERS *************************************/
// ******************************************************************************/

function* watchGetPlayerProperty(player) {
    while (true) {
        const { payload } = yield take(ASK_PLAYER_PROPERTY);
        const { property } = payload;
        yield put(getPlayerProperty(property, player[property]));
    }
}

function* watchSetPlayerProperty(player) {
    while (true) {
        const { payload } = yield take(SET_PLAYER_PROPERTY);
        const { property, value } = payload;
        player[property] = value; // eslint-disable-line
    }
}

function createPlayerFromUrl(url) {
    return window.AV.Player.fromURL(encodeURI(url));
}

function* generateActionsFromPlayerMessage(playerChannel) {
  while (true) {
      const action = yield take(playerChannel);
      yield put(action);
  }
}

function* watchPlayTrack() {
    while (true) {
        const { payload } = yield take(PLAY_TRACK);
        const player = yield call(createPlayerFromUrl, payload.track);
        const playerChannel = yield call(createPlayerChannel, player);
        const playerTasks = yield [ // need to be canceled
            fork(generateActionsFromPlayerMessage, playerChannel),
            fork(watchToggleTrack, player),
            fork(watchStopTrack, player),
            fork(watchGetPlayerProperty, player),
            fork(watchSetPlayerProperty, player),
        ];
        player.play();
        yield take(REMOVE_PREVIOUS_PLAYER);
        player.stop();
        yield playerTasks.map(task => cancel(task));
    }
}

// function* talkToPlayer(player) {
//     yield [
//         fork(watchToggleTrack, player),
//         fork(watchStopTrack, player),
//         fork(watchGetPlayerProperty, player),
//         fork(watchSetPlayerProperty, player),
//     ];
// }

function* watchStopTrack(player) {
    while (true) {
        yield take(STOP_TRACK);
        try {
            player.stop();
        } catch (error) {
            yield put(receiveError(error.message));
        }
    }
}

function* watchToggleTrack(player) {
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
    yield fork(watchPlayTrack);
}
