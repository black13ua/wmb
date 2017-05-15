import React, { PropTypes } from 'react';
import { ListSubHeader, ListDivider, Input, Button } from 'react-toolbox';


const SearchView = ({ search, onSearchClick, onSearchChange }) => {
    // function getValidationState() {
    //     const length = search.length;
    //     if (length > 2) return 'success';
    //     return 'warning';
    // }
    return (
        <div>
            <ListDivider />
            <ListSubHeader caption='Search' />
            <div>
                <Input
                    type     = 'text'
                    value    = {search}
                    icon     = 'search'
                    onChange = {onSearchChange}
                />
            </div>
            <div style = {{ width: '100%', textAlign: 'center', margin: '15px 0' }} >
                <Button
                    icon='search'
                    label='Search'
                    raised
                    primary
                    floating
                    onClick = {onSearchClick}
                />
            </div>
        </div>
    );
};


SearchView.propTypes = {
    search        : PropTypes.string,
    onSearchChange: PropTypes.func.isRequired,
    onSearchClick : PropTypes.func.isRequired,
};

export default SearchView;
