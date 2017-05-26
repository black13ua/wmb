import React from 'react';

import FiltersMenuView from '../../view/filters/main';
import RandomButtonContainer from './random-button';
import FiltersContainer from './filters/main-filter';


const RightSidebarContainer = () =>
    <FiltersMenuView>
        <RandomButtonContainer />
        <FiltersContainer />
    </FiltersMenuView>;


export default RightSidebarContainer;
