import { createSelector } from 'reselect';

const musicSelector = state => state.music;
const getTrackId = (state, props) => props.trackId;

export const makeSelectTrackDatabyId = () => createSelector(
    [musicSelector, getTrackId],
    (state, trackId) => state.data.tracks.dataById[trackId]
);

export const getSelectedTrackIds = createSelector(
    musicSelector,
    state => state.viewState.selected.tracks,
);

export const getTracksData = createSelector(
    musicSelector,
    state => state.data.tracks.dataById,
);
