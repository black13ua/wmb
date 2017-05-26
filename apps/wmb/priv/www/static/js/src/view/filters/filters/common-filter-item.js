import React, { PropTypes } from 'react';
import { ListItem } from 'react-toolbox';


const FilterItemView = ({ onClick, name }) =>
    <ListItem
        caption = {name}
        onClick = {onClick}
    />;


FilterItemView.propTypes = {
    name   : PropTypes.string,
    onClick: PropTypes.func.isRequired,
};

export default FilterItemView;
