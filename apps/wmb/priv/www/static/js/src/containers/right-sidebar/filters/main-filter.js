import React from 'react';

import FiltersView from '../../../view/right-sidebar/filters/main-filter';
import GenresFilterContainer from './genres-filter';
import DatesFilterContainer from './dates-filter';
import SearchContainer from './search';
import AbcFilterContainer from './abc-filter';


const FiltersContainer = () =>
    <FiltersView>
        <article className="genres--n--dates">
            <h3 className="filters-header"> { 'Filters:' }</h3>

            <GenresFilterContainer />
            <DatesFilterContainer />
        </article>

        <SearchContainer />
        <AbcFilterContainer />
    </FiltersView>;


export default FiltersContainer;
