import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem } from 'react-toolbox';


const FilterItemView = ({ activeClass, onClick, name }) => {
    // const itemClasses = classnames({ active: activeClass });
    return (
        <ListItem
            caption = {name}
            onClick   = {onClick}
        />
    );
};


FilterItemView.propTypes = {
    activeClass: PropTypes.bool,
    name       : PropTypes.string,
    onClick    : PropTypes.func.isRequired,
};

export default FilterItemView;
