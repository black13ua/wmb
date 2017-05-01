import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    RECEIVE_FILTER_DATA,
    RANDOM_CHECKER_TOGGLE,
    SET_FIELD_VALUE,
    SAVE_SEARCH_VALUE,
    SET_ACTIVE_ARTIST_IN_ABC_FILTER,
    RECEIVE_ARTISTS_BY_LETTER,
} from '../constants/action-types';


const initialState = Immutable({
    data: {
        filters: {
            abc   : [],
            genres: [],
            dates : [],
        },
        artistsByLetter: {},
    },
    viewState: {
        filtersCurrentValue: {
            abc   : '',
            genres: '',
            dates : '',
        },
        activeArtistId : -1,
        search         : '',
        isRandomChecked: true,
    },
});

export default createReducer(initialState, {
    [RECEIVE_FILTER_DATA](state, action) {
        const { alias, data } = action.payload;
        return state.setIn(['data', 'filters', alias], data);
    },

    [RECEIVE_ARTISTS_BY_LETTER](state, action) {
        const { letterId, data } = action.payload;
        return state.setIn(['data', 'artistsByLetter', letterId], data);
    },

    [SET_FIELD_VALUE](state, action) {
        const { alias, value } = action.payload;
        return state.merge({
            viewState: {
                filtersCurrentValue: {
                    [alias]: value,
                },
            },
        }, { deep: true });
    },

    [RANDOM_CHECKER_TOGGLE](state) {
        const currentRandomState = state.viewState.isRandomChecked;
        return state.setIn(['viewState', 'isRandomChecked'], !currentRandomState);
    },

    [SET_ACTIVE_ARTIST_IN_ABC_FILTER](state, action) {
        const { artistId } = action.payload;
        return state.setIn(['viewState', 'activeArtistId'], artistId);
    },

    [SAVE_SEARCH_VALUE](state, action) {
        const { value } = action.payload;
        return state.setIn(['viewState', 'search'], value);
    },
});
