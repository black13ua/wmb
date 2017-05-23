import Immutable from 'seamless-immutable';

import createReducer  from '../utils/createReducer';
import {
    FETCH_RANDOM_TRACKS,
    RECEIVE_ALBUMS,
    RECEIVE_RANDOM_TRACKS,
    RECEIVE_ALBUMS_BY_ARTIST,
    SELECT_TRACK,
    SELECT_ALBUM,
    CLEAR_PLAYLIST,
    CLEAR_WARNING,
    SET_WARNING,
    RECEIVE_ERROR,
    SET_CURRENT_PAGE,
    FETCHING,
    SET_ACTIVE_TRACK,
} from '../constants/action-types';


const initialState = Immutable({
    data: {
        albums: {
            ids     : [],
            dataById: {},
        },
        tracks: {
            dataById: {},
        },
    },
    viewState: {
        fetching: {
            albums: false,
            tracks: [],
            random: false,
        },
        selected: {
            albums: [],
            tracks: [],
        },
        pages: {
            back    : 0,
            previous: 0,
            current : 1,
            next    : 2,
            forward : 10,
        },
        warningMessage: '',
        maxPage       : 100,
        activeTrack   : 0,
    },
});


export default createReducer(initialState, {
    [RECEIVE_ALBUMS](state, action) {
        const { albums } = action.payload;
        const normalizedAlbums = getNormalizedAlbums(albums);
        const normalizedDataTracks = getNormalizedDataTracks(albums);
        return state
            .setIn(['data', 'albums'], normalizedAlbums)
            .merge({
                data: {
                    tracks: {
                        dataById: normalizedDataTracks,
                    },
                },
            }, { deep: true });
    },

    [FETCHING](state, action) {
        const { value, isFetching } = action.payload;
        return state
            .setIn(['viewState', 'fetching', value], isFetching);
    },

    [SET_ACTIVE_TRACK](state, action) {
        const { trackId } = action.payload;
        return state
            .setIn(['viewState', 'activeTrack'], trackId);
    },

    [RECEIVE_ALBUMS_BY_ARTIST](state, action) {
        const { albums } = action.payload;
        const normalizedAlbums = getNormalizedAlbums(albums);
        return state
            .setIn(['data', 'albums'], normalizedAlbums);
    },

    [FETCH_RANDOM_TRACKS](state) {
        return state
            .setIn(['viewState', 'fetching', 'random'], true);
    },

    [CLEAR_PLAYLIST](state) {
        return state
            .setIn(['viewState', 'selected', 'tracks'], [])
            .setIn(['viewState', 'selected', 'albums'], []);
    },

    [CLEAR_WARNING](state) {
        return state
            .setIn(['viewState', 'warningMessage'], '');
    },

    [SET_CURRENT_PAGE](state, action) {
        const { currentPage } = action.payload;
        const pages = getPages(currentPage);
        return state
            .setIn(['viewState', 'pages'], pages);
    },

    [SET_WARNING](state, action) {
        const { warning } = action.payload;
        return state
            .setIn(['viewState', 'warningMessage'], warning);
    },

    [RECEIVE_ERROR](state, action) {
        const { error } = action.payload;
        return state
            .setIn(['viewState', 'warningMessage'], error);
    },

    [SELECT_TRACK](state, action) {
        const { trackId, selected } = action.payload;
        const newSelectedTrackIds = selected
            ? _.without(state.viewState.selected.tracks, trackId)
            : state.viewState.selected.tracks.concat(trackId);

        return state
            .setIn(['viewState', 'selected', 'tracks'], newSelectedTrackIds);
    },

    [SELECT_ALBUM](state, action) {
        const { albumId, selected } = action.payload;
        const currentAlbum = state.data.albums.dataById[albumId];
        const trackIds = currentAlbum.trackIds;
        const newSelectedTrackIds = selected
            ? _.without(state.viewState.selected.tracks, ...trackIds)
            : _.union(state.viewState.selected.tracks, trackIds);

        const newSelectedAlbumIds = selected
            ? _.without(state.viewState.selected.albums, albumId)
            : state.viewState.selected.albums.concat(albumId);

        return state
            .setIn(['viewState', 'selected', 'albums'], newSelectedAlbumIds)
            .setIn(['viewState', 'selected', 'tracks'], newSelectedTrackIds);
    },

    [RECEIVE_RANDOM_TRACKS](state, action) {
        const { tracks } = action.payload;
        const normalizedTracks = getNormalizedTracks(tracks);
        const newSelectedTrackIds = state.viewState.selected.tracks.concat(normalizedTracks.ids);
        return state
            .merge({
                data: {
                    tracks: {
                        dataById: normalizedTracks.dataById,
                    },
                },
            }, { deep: true })
            .merge({
                viewState: {
                    selected: {
                        tracks: newSelectedTrackIds,
                    },
                },
            }, { deep: true })
            .setIn(['viewState', 'fetching', 'random'], false);
    },
});

// HELPERS
function getNormalizedAlbums(albums) {
    return _(albums)
        .map(album => ({ ...album, trackIds: _(album.tracks).map(track => track.trackId).compact().value() }))
        .map(album => _.omit(album, 'tracks'))
        .reduce((acum, album) => ({
            ids     : [...acum.ids, album.albumId],
            dataById: { ...acum.dataById, [album.albumId]: album },
        }), { ids: [], dataById: {} });
}

function getNormalizedDataTracks(albums) {
    return _(albums)
        .map(album => ({
            ...album,
            tracks: _(album.tracks)
                .map(track => ({ ...track, ...album }))
                .value(),
        })
        )
        .flatMap('tracks')
        .reduce((acum, track) => ({ ...acum, [track.trackId]: track }), {});
}

function getNormalizedTracks(tracks) {
    return _(tracks)
        .reduce((acum, track) => ({
            ids     : [...acum.ids, track.trackId],
            dataById: { ...acum.dataById, [track.trackId]: track },
        }), { ids: [], dataById: {} });
}

function getFirstPage(currentPage) {
    if (currentPage < 3) return 0;
    if (currentPage < 12) return 1;
    let lastPage = Math.floor(currentPage / 10) * 10;
    if (lastPage === currentPage) lastPage = currentPage - 10;
    if (lastPage === currentPage - 1)  lastPage = currentPage - 11;
    return lastPage;
}

function getPages(currentPage) {
    let lastPage = Math.ceil(currentPage / 10) * 10;
    lastPage = lastPage === currentPage ? lastPage + 10 : lastPage;
    lastPage = lastPage === (currentPage + 1) ? lastPage + 10 : lastPage;

    return {
        back    : getFirstPage(currentPage),
        previous: currentPage > 1 ? currentPage - 1 : 0,
        current : currentPage,
        next    : currentPage + 1,
        forward : lastPage,
    };
}
