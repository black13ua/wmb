import React, { PropTypes } from 'react';
import { ListSubHeader, ListDivider, Input, Button } from 'react-toolbox';


const SearchView = ({ search, onSearchClick, onSearchChange }) => {
    return (
        <div>
            <ListDivider />
            <ListSubHeader caption='Search' />
            <div>
                <Input
                    icon     = "search"
                    type     = "text"
                    value    = {search}
                    onChange = {onSearchChange}
                />
            </div>
            <div style = {{ width: '100%', textAlign: 'center', margin: '15px 0' }} >
                <Button
                    icon     = 'search'
                    label    = 'Search'
                    disabled = {search.length < 3}
                    raised
                    primary
                    floating
                    onClick  = {onSearchClick}
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
