import React from 'react';

import FiltersView from '../../../view/right-sidebar/filters/main-filter';
import CommonFilterContainer from './common-filter';
import SearchContainer from './search';


const FiltersContainer = () =>
    <FiltersView>
        <article className="genres--n--dates">
            <h3 className="filters-header"> { 'Filters:' }</h3>
            <CommonFilterContainer alias = {'genres'} />
            <CommonFilterContainer alias = {'dates'} />
        </article>

        <SearchContainer />
        <article className="abc">
            <h3 className="filters-header">{ 'Alphabetical:' }</h3>
            <CommonFilterContainer alias = {'abc'} />
        </article>
    </FiltersView>;


export default FiltersContainer;
