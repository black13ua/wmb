import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import AbcFiltersView from '../../../view/right-sidebar/filters/abc-filter';

import { fetchABCfilter } from '../../../actions';
import { getAbcFilter } from '../../../selectors';


class AbcFilterContainer extends Component {
    componentWillMount() {
        this.props.fetchAbcFilter();
    }

    handleAbcFilterChange = () => {
        console.info('handleAbcFilterChanged');
    }

    get abcOptions() {
        const list = this.props.abcFilter.map((letter, index) =>
            <option key={index} value={letter}>{ letter }</option>
        );
        return (
            <select
                id="filter-abc"
                onChange = {this.handleAbcFilterChange}
            >
                { list }
            </select>
        );
    }

    render() {
        return (
            <AbcFiltersView
                abc       = {this.abcOptions}
                abcLength = {_.size(this.props.abcFilter)}
            />
        );
    }
}

AbcFilterContainer.propTypes = {
    abcFilter     : PropTypes.arrayOf(PropTypes.string),
    fetchAbcFilter: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    abcFilter: getAbcFilter(state),
});

const mapDispatchToProps = dispatch => ({
    fetchAbcFilter: ()    => dispatch(fetchABCfilter()),
});

export default connect(mapStateToProps, mapDispatchToProps)(AbcFilterContainer);
