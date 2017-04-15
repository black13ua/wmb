import React, { PropTypes } from 'react';

const SearchView = ({ search, onSearchClick, onSearchChange }) =>
    <article className="name">
        <h3 className="filters-header">{ 'Search:' }</h3>

        <div className="filter-name">
            <label htmlFor="textSearch">
                <input
                    id          = "textSearch"
                    placeholder = "type your favorit artist"
                    type        = "text"
                    value       = {search}
                    onChange    = {onSearchChange}
                />
            </label>
            <button onClick = {onSearchClick} >{ 'find it!' }</button>
        </div>
    </article>;


SearchView.propTypes = {
    search        : PropTypes.string,
    onSearchChange: PropTypes.func.isRequired,
    onSearchClick : PropTypes.func.isRequired,
};

export default SearchView;
