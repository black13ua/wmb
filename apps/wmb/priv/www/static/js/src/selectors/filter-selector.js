import { createSelector } from 'reselect';

const filterSelector = state => state.filters;
const getAlias = (state, props) => props.alias;
const getLetterId = (state, props) => props.letterId;


export const getFilterDataByAlias = createSelector(
    [filterSelector, getAlias],
    (state, alias) => state.data.filters[alias]
);

export const getFilters = createSelector(
    filterSelector,
    state => state.data.filters
);

export const getFilterCurrentValueByAlias = createSelector(
    [filterSelector, getAlias],
    (state, alias) => state.viewState.filtersCurrentValue[alias]
);

export const getSearchValue = createSelector(
    filterSelector,
    state => state.viewState.search
);

export const getIsRandomChecked = createSelector(
    filterSelector,
    state => state.viewState.isRandomChecked
);

export const getArtistsByLetter = createSelector(
    [filterSelector, getLetterId],
    (state, letterId) => state.data.artistsByLetter[letterId]
);
