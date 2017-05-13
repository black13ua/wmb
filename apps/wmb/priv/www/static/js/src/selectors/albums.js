import { createSelector } from 'reselect';

const musicSelector = state => state.music;
const getAlbumId = (state, props) => props.id;


export const getAlbumsIds = createSelector(
    musicSelector,
    state => state.data.albums.ids
);

export const getCurrentPage = createSelector(
    musicSelector,
    state => state.viewState.currentPage
);

export const makeSelectAlbumDatabyId = () => createSelector(
    [musicSelector, getAlbumId],
    (state, albumId) => state.data.albums.dataById[albumId]
);
