import { createSelector } from 'reselect';
import { get } from 'lodash';

const filterSelector = state => state.filters;
const getAlias = (state, props) => props.alias;
const getLetterId = (state, props) => props.letterId;


export const getFilterDataByAlias = createSelector(
    [filterSelector, getAlias],
    (state, alias) => get(state, ['data', 'filters', alias])
);

export const getFilters = createSelector(
    filterSelector,
    state => get(state, ['data', 'filters'])
);

export const getFilterSelectedValuesByAlias = createSelector(
    [filterSelector, getAlias],
    (state, alias) => get(state, ['viewState', 'filtersCurrentValue', alias])
);

export const getFilterSelectedValues = createSelector(
    [filterSelector],
    state => get(state, ['viewState', 'filtersCurrentValue'])
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

export const getActiveArtist = createSelector(
    filterSelector,
    state => state.viewState.activeArtistId
);

export const getRandomNumber = createSelector(
    filterSelector,
    state => state.data.randomNumber
);
