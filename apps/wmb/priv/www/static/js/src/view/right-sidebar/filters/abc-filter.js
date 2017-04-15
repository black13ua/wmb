import React, { PropTypes } from 'react';

const AbcFilterView = ({ abc, abcLength }) =>
    <article className="abc">
        <h3 className="filters-header">{ 'Alphabetical:' }</h3>

        <div className="filter-abc">
            <label htmlFor="filter-abc">
                <b>{ abcLength }</b>
                <span>{ 'Letters:' }</span>
                { abc }
            </label>
        </div>
    </article>;


AbcFilterView.propTypes = {
    abc      : PropTypes.node,
    abcLength: PropTypes.number,
};

export default AbcFilterView;
