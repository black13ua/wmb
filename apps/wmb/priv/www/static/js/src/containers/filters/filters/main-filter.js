import React from 'react';
import { List } from 'react-toolbox';

import FiltersView from '../../../view/filters/filters/main-filter';
import SearchContainer from './search';
import CommonFilterContainer from './common-filter';
import AbcFilterContainer from './abc-filter';


const FiltersContainer = () =>
    <FiltersView>
        <List>
            <CommonFilterContainer alias = "genres" />
            <CommonFilterContainer alias = "dates" />
            <AbcFilterContainer alias = "abc" />
        </List>
        <SearchContainer />
    </FiltersView>;


export default FiltersContainer;
