import React, { PropTypes } from 'react';
import translate from '../../../../constants/names';

const AbcFilterView = ({ onClick, optionsLength, alias, children }) =>
    <div className={`filter-${alias}`}>
        <label htmlFor={`filter-${alias}`}>
            <b>{ optionsLength }</b>
            <button onClick = {onClick}>{ translate.filterHeaders[alias] || alias }</button>
            { children }
        </label>
    </div>;


AbcFilterView.propTypes = {
    alias        : PropTypes.string.isRequired,
    children     : PropTypes.node,
    optionsLength: PropTypes.number.isRequired,
    onClick      : PropTypes.func.isRequired,
};

export default AbcFilterView;
