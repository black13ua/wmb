import React, { Component } from 'react';
import { connect } from 'react-redux';

import FilterView from '../../components/filters/filters-view';


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

const mapDispatchToProps = () => ({
});


export default connect(mapStateToProps, mapDispatchToProps())(FiltersContainer);
