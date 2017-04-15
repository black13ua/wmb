import React from 'react';

import RightSidebarView from '../../view/right-sidebar/main';
import RandomButtonContainer from './random-button';
import FiltersContainer from './filters/main-filter';


const RightSidebarContainer = () =>
    <RightSidebarView>
        <RandomButtonContainer />
        <FiltersContainer />
    </RightSidebarView>;


export default RightSidebarContainer;
