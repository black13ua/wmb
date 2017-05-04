import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { ListGroupItem } from 'react-bootstrap';


const FilterItemView = ({ children, activeClass, onClick }) => {
    const itemClasses = classnames({ active: activeClass });
    return (
        <ListGroupItem
            className = {itemClasses}
            type      = "button"
            onClick   = {onClick}
        >
            { children }
        </ListGroupItem>
    );
};


FilterItemView.propTypes = {
    activeClass: PropTypes.bool,
    children   : PropTypes.node,
    onClick    : PropTypes.func.isRequired,
};

export default FilterItemView;
