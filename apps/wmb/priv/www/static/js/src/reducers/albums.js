import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    RECEIVE_ALBUMS,
    RECEIVE_RANDOM_TRACKS,
    RECEIVE_ALBUMS_BY_ARTIST,
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
    [RECEIVE_ALBUMS](state, action) {
        const { albums } = action.payload;
        const normalizedAlbums = getNormalizedAlbums(albums);
        console.info('normalizedAlbums', normalizedAlbums);
        return state;
    },

    [RECEIVE_ALBUMS_BY_ARTIST](state, action) {
        const { albums } = action.payload;
        const normalizedAlbums = getNormalizedAlbums(albums);
        console.info('normalizedAlbums', normalizedAlbums);
        return state;
    },

    [RECEIVE_RANDOM_TRACKS](state, action) {
        console.log('%c fix it! >> RECEIVE_RANDOM_TRACKS', 'color: red');
        return state;
        // const currentSongs = state.data.songs;
        // return state.merge({
        //     data: {
        //         songs: [...currentSongs, ...action.payload.tracks],
        //     },
        // }, { deep: true });
    },
});

// HELPERS
function getNormalizedAlbums(albums) {
    return albums;
}