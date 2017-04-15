import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    RECEIVE_ABC_FILTER,
    RECEIVE_GENRES_FILTER,
    RECEIVE_DATES_FILTER,
    RANDOM_CHECKER_TOGGLE,
    RECEIVE_RANDOM_TRACKS,
} from '../constants/action-types';


const initialState = Immutable({
    data: {
        albums : [],
        songs  : [],
        filters: {
            abc   : ['A', 'B', 'C'],
            genres: ['Rock', 'Folk'],
            dates : ['1985', '2017'],
        },
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
            byGenre: '',
            byYear : '',
            search : '',
            byABC  : '',
        },
        isRandomChecked: true,
    },
});

export default createReducer(initialState, {
    [RECEIVE_ABC_FILTER](state, action) {
        return state.setIn(['data', 'filters', 'abc'], action.payload.abc);
    },

    [RECEIVE_GENRES_FILTER](state, action) {
        return state.setIn(['data', 'filters', 'genres'], action.payload.genres);
    },

    [RECEIVE_DATES_FILTER](state, action) {
        return state.setIn(['data', 'filters', 'dates'], action.payload.dates);
    },

    [RECEIVE_RANDOM_TRACKS](state, action) {
        const currentSongs = state.data.songs;
        return state.merge({
            data: {
                songs: [...currentSongs, ...action.payload.tracks],
            },
        }, { deep: true });
    },

    [RANDOM_CHECKER_TOGGLE](state) {
        const currentRandomState = state.viewState.isRandomChecked;
        return state.setIn(['viewState', 'isRandomChecked'], !currentRandomState);
    },
});
