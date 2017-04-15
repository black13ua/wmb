import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import SearchView from '../../../view/right-sidebar/filters/search';

import { fetchSearchResults, saveSearchValue } from '../../../actions';
import { getSearchValue } from '../../../selectors';


const SearchContainer = ({ search, handleSearchClick, handleSearchChange }) =>
    <SearchView
        search         = {search}
        onSearchChange = {handleSearchChange}
        onSearchClick  = {handleSearchClick}
    />;


SearchContainer.propTypes = {
    handleSearchChange: PropTypes.func.isRequired,
    handleSearchClick : PropTypes.func.isRequired,
    search            : PropTypes.string,
};

const mapStateToProps = state => ({
    search: getSearchValue(state),
});

const mapDispatchToProps = dispatch => ({
    handleSearchClick : ()    => dispatch(fetchSearchResults()),
    handleSearchChange: value => dispatch(saveSearchValue(value)),
});


export default connect(mapStateToProps, mapDispatchToProps)(SearchContainer);
