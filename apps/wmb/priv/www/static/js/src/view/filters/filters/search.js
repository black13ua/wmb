import React, { PropTypes } from 'react';
import { ListSubHeader, ListDivider, Input, Button } from 'react-toolbox';


const SearchView = ({ search, onSearchClick, onSearchChange }) =>
    <div>
        <ListDivider />
        <ListSubHeader caption="Search" />
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
                floating
                primary
                raised
                disabled = {search.length < 3}
                icon     = "search"
                label    = "Search"
                onClick  = {onSearchClick}
            />
        </div>
    </div>;


SearchView.propTypes = {
    search        : PropTypes.string,
    onSearchChange: PropTypes.func.isRequired,
    onSearchClick : PropTypes.func.isRequired,
};

export default SearchView;
