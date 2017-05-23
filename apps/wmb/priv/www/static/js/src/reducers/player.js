import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    ON_PLAYER_BUFFER,
    ON_PLAYER_DURATION,
    ON_PLAYER_PROGRESS,
    GET_PLAYER_PROPERTY,
} from '../constants/action-types';

const initialState = Immutable({
    progress: 0,
    buffer  : 0,
    duration: 0,
    playing : false,
});

export default createReducer(initialState, {
    [ON_PLAYER_PROGRESS](state, action) {
        const { progress } = action.payload;
        return state.set('progress', progress);
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
});
