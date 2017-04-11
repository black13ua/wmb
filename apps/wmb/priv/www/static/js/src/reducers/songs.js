import Immutable from 'seamless-immutable';
// import * as actionTypes from '../../constants/live/live';
// import * as socketActionTypes from '../../constants/common/socket';


const initialState = Immutable(
    {
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
            filters: {
                byName: false,
                byYear: false,
            },
        },
    }
);


export default function songsReducer(state = initialState, action = {}) {
    switch (action.type) {
        default:
            return state;
    }
}
