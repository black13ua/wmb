import React, { PropTypes } from 'react';

const DateFilterView = ({ dates, datesLength }) =>
    <div className="filter-date">
        <label htmlFor="filter-date">
            <b>{ datesLength }</b><span>{ 'Dates:' }</span>
            { dates }
        </label>
    </div>;


DateFilterView.propTypes = {
    dates      : PropTypes.node,
    datesLength: PropTypes.number,
};

export default DateFilterView;
