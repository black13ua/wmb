import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    RECEIVE_FILTER_DATA,
    RANDOM_CHECKER_TOGGLE,
    RECEIVE_RANDOM_TRACKS,
    SET_FIELD_VALUE,
    SAVE_SEARCH_VALUE,
} from '../constants/action-types';


const initialState = Immutable({
    data: {
        filters: {
            abc   : ['A', 'B', 'C'],
            genres: ['Rock', 'Folk'],
            dates : ['1985', '2017'],
        },
    },
    viewState: {
        filtersCurrentValue: {
            abc   : '',
            genres: '',
            dates : '',
        },
        search         : '',
        isRandomChecked: true,
    },
});

export default createReducer(initialState, {
    [RECEIVE_FILTER_DATA](state, action) {
        const { alias, data } = action.payload;
        return state.merge({
            data: {
                filters: {
                    [alias]: data,
                },
            },
            viewState: {
                filtersCurrentValue: {
                    [alias]: 'All',
                },
            },
        }, { deep: true });
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

    [SAVE_SEARCH_VALUE](state, action) {
        const { value } = action.payload;
        return state.setIn(['viewState', 'search'], value);
    },
});
