import { createSelector } from 'reselect';
import { getTracksData } from './tracks';

const musicSelector = state => state.music;

export const getFetchingState = createSelector(
    musicSelector,
    state => state.viewState.fetching
);

export const getCurrentPage = createSelector(
    musicSelector,
    state => state.viewState.currentPage
);

export const getWarningMessage = createSelector(
    musicSelector,
    state => state.viewState.warningMessage
);

export const getActiveTrackId = createSelector(
    musicSelector,
    state => state.viewState.activeTrack
);

export const getActiveTrackData = createSelector(
    [getTracksData, getActiveTrackId],
    (tracksData, activeTrackId) => _.get(tracksData, activeTrackId, {})
);
