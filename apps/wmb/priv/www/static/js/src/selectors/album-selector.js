import { createSelector } from 'reselect';

const albumsSelector = state => state.albums;
const getAlbumId = (state, props) => props.id;


export const getAlbumsIds = createSelector(
    albumsSelector,
    state => state.data.albums.ids
);

export const getCurrentPage = createSelector(
    albumsSelector,
    state => state.viewState.currentPage
);
