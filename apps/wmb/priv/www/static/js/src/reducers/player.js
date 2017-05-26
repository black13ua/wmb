import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    ON_PLAYER_BUFFER,
    ON_PLAYER_DURATION,
    ON_PLAYER_PROGRESS,
    GET_PLAYER_PROPERTY,
    PLAY_TRACK,
    SET_STORE_PROPERTY,
} from '../constants/action-types';

const initialState = Immutable({
    progress: 0,
    buffer  : 0,
    duration: 0,
    playing : false,
    volume  : 50,
});

export default createReducer(initialState, {
    [PLAY_TRACK](state) {
        return state
            .set('buffer', 0)
            .set('progress', 0);
    },

    [ON_PLAYER_PROGRESS](state, action) {
        if (!state.duration) return state;

        const { progress } = action.payload;
        const percentProgress = ((progress * 100) / state.duration).toFixed(2);
        return state.set('progress', percentProgress);
    },

    [ON_PLAYER_BUFFER](state, action) {
        const { buffer } = action.payload;
        return state.set('buffer', buffer);
    },

    [ON_PLAYER_DURATION](state, action) {
        const { duration } = action.payload;
        return state.set('duration', duration);
    },

    [GET_PLAYER_PROPERTY](state, action) {
        const { property, value } = action.payload;
        return state.set(property, value);
    },

    [SET_STORE_PROPERTY](state, action) {
        const { property, value } = action.payload;
        return state.set(property, value);
    },
});
