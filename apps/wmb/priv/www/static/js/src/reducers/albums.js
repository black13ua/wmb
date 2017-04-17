import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    RECEIVE_DATA,
} from '../constants/action-types';


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
        currentPage: 1,
    },
});

export default createReducer(initialState, {
    [RECEIVE_DATA](state) {
        const currentRandomState = state.viewState.isRandomChecked;
        return state.setIn(['viewState', 'isRandomChecked'], !currentRandomState);
    },
});
