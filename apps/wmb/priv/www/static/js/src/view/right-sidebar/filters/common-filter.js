import React, { PropTypes } from 'react';
import translate from '../../../constants/names';

const FilterView = ({ select, optionsLength, alias }) =>
    <div className={`filter-${alias}`}>
        <label htmlFor={`filter-${alias}`}>
            <b>{ optionsLength }</b><span>{ translate.filterHeaders[alias] || alias }</span>
            { select }
        </label>
    </div>;


FilterView.propTypes = {
    alias        : PropTypes.string.isRequired,
    optionsLength: PropTypes.number.isRequired,
    select       : PropTypes.node,
};

export default FilterView;
