import React, { PropTypes } from 'react';
import classnames from 'classnames';


const FilterItemView = ({ children, activeClass, onClick }) => {
    const itemClasses = classnames('filter--item', { active: activeClass });
    return (
        <li
            className = {itemClasses}
            onClick   = {onClick}
        >
            { children }
        </li>
    );
};


FilterItemView.propTypes = {
    activeClass: PropTypes.bool,
    children   : PropTypes.node,
    onClick    : PropTypes.func.isRequired,
};

export default FilterItemView;
