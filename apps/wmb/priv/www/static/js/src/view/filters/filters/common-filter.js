import React, { PropTypes } from 'react';
import { ListItem } from 'react-toolbox';

import translate from '../../../constants/names';

const iconsByAlias = {
    genres: 'library_music',
    dates : 'history',
    abc   : 'line_weight',
};


const FilterView = ({ children, optionsLength, alias, onClick, activeClass }) =>
    <div style = {{ cursor: 'pointer' }}>
        <ListItem
            caption   = {`${translate.filterHeaders[alias] || alias}`}
            leftIcon  = {iconsByAlias[alias] || 'assignment'}
            rightIcon = {activeClass ? 'keyboard_arrow_down' : 'keyboard_arrow_right'}
            onClick   = {onClick}
        />
        <div style = {{ marginLeft: '35px' }}>
            { children }
        </div>
    </div>;


FilterView.propTypes = {
    activeClass  : PropTypes.bool,
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    onClick      : PropTypes.func.isRequired,
};

export default FilterView;
