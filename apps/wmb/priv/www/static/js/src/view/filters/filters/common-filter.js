import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem } from 'react-toolbox';

import translate from '../../../constants/names';

const iconsByAlias = {
    genres: 'library_music',
    dates : 'history',
    abc   : 'line_weight',
};

const FilterView = ({ children, optionsLength, alias, onClick, activeClass, fetching }) => {
    return (
        <div style = {{ cursor: 'pointer' }}>
            <ListItem
                caption={`${translate.filterHeaders[alias] || alias}`}
                onClick = {onClick}
                rightIcon={activeClass ? 'keyboard_arrow_down' : 'keyboard_arrow_right'}
                leftIcon={iconsByAlias[alias] || 'assignment'}
            />
            <div style = {{ textAlign: 'center' }}>
                { children }
            </div>
        </div>
    );
};


FilterView.propTypes = {
    activeClass  : PropTypes.bool,
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    fetching     : PropTypes.object,
    onClick      : PropTypes.func.isRequired,
};

export default FilterView;
