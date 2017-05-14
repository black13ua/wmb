import React, { PropTypes } from 'react';
import Input from 'react-toolbox/lib/input';


const SearchView = ({ search, onSearchClick, onSearchChange }) => {
    function getValidationState() {
        const length = search.length;
        if (length > 2) return 'success';
        return 'warning';
    }

    return (
        <article className="name">
            <Input
                type='text'
                label='find artist'
                value={search}
                onChange={onSearchChange}
                icon='search'
            />
        </article>
    );
};


SearchView.propTypes = {
    search        : PropTypes.string,
    onSearchChange: PropTypes.func.isRequired,
    onSearchClick : PropTypes.func.isRequired,
};

export default SearchView;
