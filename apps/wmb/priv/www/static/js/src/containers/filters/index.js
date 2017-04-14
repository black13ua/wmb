import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import FilterView from '../../components/filters/filters-view';
import { fetchABCfilter } from '../../actions';
import { getAbcFilter } from '../../selectors';


class FiltersContainer extends Component {
    componentWillMount() {
        this.props.fetchABC();
    }

    get renderAbcFilter() {
        const list = this.props.abcFilter.map((letter, index) =>
            <option key={index} value={`${letter}`}>{ letter }</option>
        );

        return (
            <select
                id   = "filter-abc"
                name = "filter-abc"
            >
                { list }
            </select>
        );
    }

    render() {
        return (
            <FilterView
                abc = {this.renderAbcFilter}
            />
        );
    }
}

FiltersContainer.propTypes = {
    fetchABC : PropTypes.func.isRequired,
    abcFilter: PropTypes.arrayOf(PropTypes.string),
};

const mapStateToProps = state => ({
    abcFilter: getAbcFilter(state),
});

const mapDispatchToProps = dispatch => ({
    fetchABC: () => dispatch(fetchABCfilter()),
});


export default connect(mapStateToProps, mapDispatchToProps)(FiltersContainer);
