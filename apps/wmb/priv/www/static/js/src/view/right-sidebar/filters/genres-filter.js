import React, { PropTypes } from 'react';

const GenresFilterView = ({ genres, genresLength }) =>
    <div className="filter-genres">
        <label htmlFor="filter-genres">
            <b>{ genresLength }</b><span>{ 'Genres:' }</span>
            { genres }
        </label>
    </div>;


GenresFilterView.propTypes = {
    genres      : PropTypes.node,
    genresLength: PropTypes.number,
};

export default GenresFilterView;
