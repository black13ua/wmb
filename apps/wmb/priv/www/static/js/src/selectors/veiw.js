import { createSelector } from 'reselect';


const musicSelector = state => state.music;

export const getFetchingState = createSelector(
    musicSelector,
    state => state.viewState.fetching
);
