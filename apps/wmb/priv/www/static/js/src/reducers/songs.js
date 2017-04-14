import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import { RECEIVE_ABC_FILTER } from '../constants/action-types';


const initialState = Immutable({
    data: {
        albums: {
            ids     : [],
            dataById: {},
        },
        songs: {
            ids     : [],
            dataById: {},
        },
        abc: [],
    },
    viewState: {
        isFetching: {
            albums: [],
            songs : [],
        },
        selected: {
            albums: [],
            songs : [],
        },
        filters: {
            byName: false,
            byYear: false,
        },
    },
});


export default createReducer(initialState, {
    [RECEIVE_ABC_FILTER](state, action) {
        return state.setIn(['data', 'abc'], action.payload.abc);
    },
});
