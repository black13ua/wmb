import { createSelector } from 'reselect';

const musicSelector = state => state.music;
const getAlbumId = (state, props) => props.albumId;


export const getAlbumsIds = createSelector(
    musicSelector,
    state => state.data.albums.ids
);

export const getSelectedAlbumIds = createSelector(
    musicSelector,
    state => state.viewState.selected.albums,
);

export const makeSelectAlbumDatabyId = () => createSelector(
    [musicSelector, getAlbumId],
    (state, albumId) => state.data.albums.dataById[albumId]
);
