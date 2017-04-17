import { createSelector } from 'reselect';

const filterSelector = state => state.filters;
const filterAlias = (state, props) => props.alias;


export const getFilterDataByAlias = createSelector(
    [filterSelector, filterAlias],
    (state, alias) => state.data.filters[alias]
);

export const getFilterCurrentValueByAlias = createSelector(
    [filterSelector, filterAlias],
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
