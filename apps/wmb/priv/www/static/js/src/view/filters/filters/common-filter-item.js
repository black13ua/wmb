import React, { PropTypes } from 'react';
import { Checkbox } from 'react-toolbox';


const FilterItemView = ({ id, name, checked, onChange }) =>
    <Checkbox
        checked  = {checked}
        label    = {name}
        onChange = {onChange.bind(this, id, checked)}
    />;


FilterItemView.propTypes = {
    name    : PropTypes.string.isRequired,
    checked : PropTypes.bool,
    onChange: PropTypes.func.isRequired,
};

export default FilterItemView;
