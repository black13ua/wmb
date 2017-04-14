import { createSelector } from 'reselect';

const filterSelector = state => state.songs;

export const getAbcFilter = createSelector(
    filterSelector,
    state => state.data.abc
);

