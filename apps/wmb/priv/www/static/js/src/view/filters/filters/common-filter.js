import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem } from 'react-toolbox';

import translate from '../../../constants/names';


const FilterView = ({ children, optionsLength, alias, onClick, activeClass }) => {
    return (
        <div>
            <ListItem
                caption={`${translate.filterHeaders[alias] || alias}`}
                onClick = {onClick}
                rightIcon="keyboard_arrow_right"
                leftIcon='assignment'
            />
            { children }
        </div>
    );
};


FilterView.propTypes = {
    activeClass  : PropTypes.bool,
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    onClick      : PropTypes.func.isRequired,
};

export default FilterView;
