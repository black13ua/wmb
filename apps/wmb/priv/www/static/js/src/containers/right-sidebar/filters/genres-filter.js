import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import GenresFilterView from '../../../view/right-sidebar/filters/genres-filter';

import { fetchGenresFilter } from '../../../actions';
import { getGenresFilter } from '../../../selectors';


class GenresFilterContainer extends Component {
    componentWillMount() {
        this.props.initFetchGenresFilter();
    }

    handleGenresFilterChange = () => {
        console.info('handleGenresFilterChange');
    }

    get genresOptions() {
        const list = this.props.genresFilter.map((genre, index) =>
            <option key={index} value={genre}>{ genre }</option>
        );

        return (
            <select
                id="filter-genres"
                onChange = {this.handleGenresFilterChange}
            >
                { list }
            </select>
        );
    }

    render() {
        return (
            <GenresFilterView
                genres       = {this.genresOptions}
                genresLength = {_.size(this.props.genresFilter)}
            />
        );
    }
}

GenresFilterContainer.propTypes = {
    genresFilter         : PropTypes.arrayOf(PropTypes.string),
    initFetchGenresFilter: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    genresFilter: getGenresFilter(state),
});

const mapDispatchToProps = dispatch => ({
    initFetchGenresFilter: () => dispatch(fetchGenresFilter()),
});


export default connect(mapStateToProps, mapDispatchToProps)(GenresFilterContainer);
