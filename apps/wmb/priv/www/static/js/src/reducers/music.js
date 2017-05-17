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
            albums: [],
            tracks: [],
            random: false,
        },
        selected: {
            albums: [],
            tracks: [],
        },
        currentPage: 1,
    },
});

export default createReducer(initialState, {
    [RECEIVE_ALBUMS](state, action) {
        const { albums } = action.payload;
        const normalizedAlbums = getNormalizedAlbums(albums);
        const normalizedDataTracks = getNormalizedDataTracks(albums);
        return state
            .setIn(['data', 'albums'], normalizedAlbums)
            .setIn(['data', 'tracks', 'dataById'], normalizedDataTracks);
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
            .setIn(['viewState', 'selected', 'tracks'], []);
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
        .map(album => ({ ...album, 'trackIds': _(album.tracks).map(track => track.trackId).compact().value() }))
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
