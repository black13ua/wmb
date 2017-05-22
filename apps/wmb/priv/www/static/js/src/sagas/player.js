import { put, fork, take } from 'redux-saga/effects';

// import * as API from '../../api';
import { PLAY_TRACK, STOP_TRACK, TOGGLE_TRACK } from '../constants/action-types';
import {
    receiveError,
    onPlayerBuffer,
    onPlayerDuration,
    onPlayerProgress,
    onPlayerEnd,
} from '../actions';

let player = null;
function* addHandlersToPlayer() {
    while (true) {
        if (player) {
            player.on('buffer', buffer     => yield put(onPlayerBuffer(buffer)));
            player.on('duration', duration => yield put(onPlayerDuration(duration)));
            player.on('progress', progress => yield put(onPlayerProgress(progress)));
            player.on('error', error       => yield put(receiveError(error)));
            player.on('end', ()            => yield put(onPlayerEnd()));
        }
        yield take('REINIT_PLAYER_HANDLRES');
    }
}
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
        yield fork(addHandlersToPlayer, player);
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

import { eventChannel } from 'redux-saga';

export default function createSocketChannel(socket) {
    // `eventChannel` takes a subscriber function
    // the subscriber function takes an `emit` argument to put messages onto the channel
  return eventChannel(emit => {
    const messageHandler = (socketMessage) => {
      let messages = [].concat(JSON.parse(socketMessage.data)); // can be both types: object and array

      // messages = reduceOutcomeUpdateList(messages);

      messages.forEach((singleMessage) => {
        const processedMessage = socketMessageProcessor(singleMessage);
        emit(processedMessage);
      });
    };

    socket.onopen = () => emit(actionCreatorFactory(SOCKET_ON_OPEN));
    socket.onmessage = messageHandler; // eslint-disable-line
    socket.onclose = () => emit(actionCreatorFactory(SOCKET_ON_CLOSE));
    const unsubscribe = () => socket.close();

    return unsubscribe;
  });
}


export default function* () {
  while (true) { // eslint-disable-line
    const socket = yield call(createWebSocketConnection);
    const socketChannel = yield call(createSocketChannel, socket);
    const socketTask  = yield [
      fork(generateActionsFromSocketMessage, socketChannel),
      fork(readAndWriteToSocket, socket),
    ];
    yield take(socketActions.SOCKET_ON_CLOSE);
    yield cancel(socketTask);
    yield call(socketChannel);
  }
}


function* generateActionsFromSocketMessage(socketChannel) {
  while (true) { // eslint-disable-line
    const action = yield take(socketChannel);
    yield put(action);
  }
}
