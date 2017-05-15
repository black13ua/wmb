import React, { PropTypes } from 'react';
import { List, Input } from 'react-toolbox';


const SearchView = ({ search, onSearchClick, onSearchChange }) => {
    // function getValidationState() {
    //     const length = search.length;
    //     if (length > 2) return 'success';
    //     return 'warning';
    // }
    return (
        <List>
            <Input
                type     = 'text'
                value    = {search}
                icon     = 'search'
                onChange = {onSearchChange}
            />
        </List>
    );
};


SearchView.propTypes = {
    search        : PropTypes.string,
    onSearchChange: PropTypes.func.isRequired,
    onSearchClick : PropTypes.func.isRequired,
};

export default SearchView;
