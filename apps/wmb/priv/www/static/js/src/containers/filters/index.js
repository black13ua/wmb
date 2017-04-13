import React, { Component } from 'react';
import { connect } from 'react-redux';

import FilterView from '../../components/filters/filters-view';
import { fetchABCfilter } from '../../actions';


class FiltersContainer extends Component {
    render() {
        return (
            <FilterView />
        );
    }
}

FiltersContainer.propTypes = {

};

const mapStateToProps = () => ({
});

const mapDispatchToProps = dispatch => ({
    fetchABC: () => dispatch(fetchABCfilter()),
});


export default connect(mapStateToProps, mapDispatchToProps)(FiltersContainer);
