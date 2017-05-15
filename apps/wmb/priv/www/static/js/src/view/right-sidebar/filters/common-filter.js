import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem, ListSubHeader } from 'react-toolbox';

import translate from '../../../constants/names';


const FilterView = ({ children, optionsLength, alias, onClick, activeClass }) => {
    return (
        <ListItem>
            <ListSubHeader caption='Random'
                onClick = {onClick}
            >
                <span className="badge" >
                    {`${optionsLength}`}
                </span>
                {`${translate.filterHeaders[alias] || alias}`}
                <i className="fa fa-caret-down" />
            </ListSubHeader>
            <div>
                { children }
            </div>
        </ListItem>
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
