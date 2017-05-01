import React from 'react';

import FiltersView from '../../../view/right-sidebar/filters/main-filter';
import SearchContainer from './search';
// import RcMenuContainer from './rc-menu';
import CommonFilterContainer from './common-filter';
import AbcFilterContainer from './abc-filter';


const FiltersContainer = () =>
    <FiltersView>
        <article className="genres--n--dates">
            <h3 className="filters-header"> { 'Filters:' }</h3>
            <CommonFilterContainer alias = "genres" />
            <CommonFilterContainer alias = "dates" />
            <AbcFilterContainer alias = "abc" />
        </article>

        <SearchContainer />
    </FiltersView>;


export default FiltersContainer;
