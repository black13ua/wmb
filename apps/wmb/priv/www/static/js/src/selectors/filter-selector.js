import { createSelector } from 'reselect';

const filterSelector = state => state.songs;

export const getAbcFilter = createSelector(
    filterSelector,
    state => state.data.filters.abc
);

export const getGenresFilter = createSelector(
    filterSelector,
    state => state.data.filters.genres
);

export const getDatesFilter = createSelector(
    filterSelector,
    state => state.data.filters.dates
);


export const getSearchValue = createSelector(
    filterSelector,
    state => state.viewState.filters.search
);

export const getIsRandomChecked = createSelector(
    filterSelector,
    state => state.viewState.isRandomChecked
);
