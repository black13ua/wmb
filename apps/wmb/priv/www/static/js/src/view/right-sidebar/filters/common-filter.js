import React, { PropTypes } from 'react';
import classnames from 'classnames';
import translate from '../../../constants/names';


const FilterView = ({ children, optionsLength, alias, onClick, activeClass }) => {
    const itemClasses = classnames('sub--menu--header', { active: activeClass });
    return (
        <div onClick = {onClick}>
            <span className = {itemClasses} >
                <b className="number">{`${optionsLength}`}</b>
                <span>{`${translate.filterHeaders[alias] || alias}`}</span>
            </span>
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
