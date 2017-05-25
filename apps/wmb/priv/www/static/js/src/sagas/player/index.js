import { delay } from 'redux-saga';
import { put, fork, take, call, cancel, select } from 'redux-saga/effects';
import createPlayerChannel from './player-event-emiter';

import {
    PLAY_TRACK,
    STOP_TRACK,
    TOGGLE_TRACK,
    ASK_PLAYER_PROPERTY,
    SET_PLAYER_PROPERTY,
    REMOVE_PREVIOUS_PLAYER,
    ON_PLAYER_END,
    NEXT_TRACK,
    PREV_TRACK,
    RECEIVE_PLAYER_ERROR,
} from '../../constants/action-types';

import {
    receiveError,
    getPlayerProperty,
    setActiveTrack,
    playTrack,
    askPlayerProperty,
    fetchRandomTracks,
    setPlayerProperty,
} from '../../actions';

import {
    getActiveTrackId,
    getSelectedTrackIds,
    makeSelectTrackDatabyId,
    getIsRandomChecked,
    getPlayerVolume,
    getRepeatPlaylistStatus,
} from '../../selectors';
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
        yield call(delay, 100); // for watchSetPlayerProperty
        const volume = yield select(getPlayerVolume);
        yield put(setPlayerProperty('volume', volume));
        player.play();
        yield put(askPlayerProperty('playing'));
        yield take(REMOVE_PREVIOUS_PLAYER);
        player.stop();
        yield playerTasks.map(task => cancel(task));
    }
}

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

function* watchTrackEnd() {
    while (true) {
        yield take([ON_PLAYER_END, NEXT_TRACK, RECEIVE_PLAYER_ERROR]);
        try {
            yield put({ type: REMOVE_PREVIOUS_PLAYER });
            const activeTrack = yield select(getActiveTrackId);
            if (activeTrack) {
                const selectedTracks = yield select(getSelectedTrackIds);
                const activeIndex = _.indexOf(selectedTracks, activeTrack);
                const autoload = yield select(getIsRandomChecked);
                if ((activeIndex === selectedTracks.length - 2) && autoload) {
                    yield put(fetchRandomTracks()); // load more random tracks
                }
                let nextActiveIndex;
                if (activeIndex === selectedTracks.length - 1) {
                    const repeating = yield select(getRepeatPlaylistStatus);
                    if (repeating) {
                        nextActiveIndex = 0;
                    } else {
                        throw new Error('Playlist is over, check out repeat button.');
                    }
                } else {
                    nextActiveIndex = activeIndex + 1;
                }
                const trackId = _.get(selectedTracks, nextActiveIndex, 0);
                yield put(setActiveTrack(trackId));
                const getTrackDataById = makeSelectTrackDatabyId();
                const track = yield select(state => getTrackDataById(state, { trackId }));
                yield put(playTrack(track.file));
            }
        } catch (error) {
            yield put(receiveError(error.message));
        }
    }
}

function* watchPrevTrack() {
    while (true) {
        yield take(PREV_TRACK);
        try {
            yield put({ type: REMOVE_PREVIOUS_PLAYER });
            const activeTrack = yield select(getActiveTrackId);
            if (activeTrack) {
                const selectedTracks = yield select(getSelectedTrackIds);
                const activeIndex = _.indexOf(selectedTracks, activeTrack);
                let nextActiveIndex;
                if (activeIndex === 0) {
                    const repeating = yield select(getRepeatPlaylistStatus);
                    if (repeating) {
                        nextActiveIndex = selectedTracks.length - 1;
                    } else {
                        throw new Error('Playlist is over, check out repeat button.');
                    }
                } else {
                    nextActiveIndex = activeIndex - 1;
                }
                const trackId = _.get(selectedTracks, nextActiveIndex, 0);
                yield put(setActiveTrack(trackId));
                const getTrackDataById = makeSelectTrackDatabyId();
                const track = yield select(state => getTrackDataById(state, { trackId }));
                yield put(playTrack(track.file));
            }
        } catch (error) {
            yield put(receiveError(error.message));
        }
    }
}

export default function* () {
    yield [
        fork(watchPlayTrack),
        fork(watchTrackEnd),
        fork(watchPrevTrack),
    ];
}
