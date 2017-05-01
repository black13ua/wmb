import React from 'react';
import { alert, listGroup, badge } from 'bootstrap-css';
import CSSModules from 'react-css-modules';

import FiltersView from '../../../view/right-sidebar/filters/main-filter';
import SearchContainer from './search';
import CommonFilterContainer from './common-filter';
import AbcFilterContainer from './abc-filter';

const styles = {};
Object.assign(styles, alert, listGroup, badge);


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


export default CSSModules(FiltersContainer, styles, { allowMultiple: true });
