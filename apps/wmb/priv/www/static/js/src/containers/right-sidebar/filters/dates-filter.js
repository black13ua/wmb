import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import DatesFilterView from '../../../view/right-sidebar/filters/date-filter';

import { fetchDatesFilter } from '../../../actions';
import { getDatesFilter } from '../../../selectors';


class DatesFilterContainer extends Component {
    componentWillMount() {
        this.props.initFetchDatesFilter();
    }

    handleDateFilterChanged = () => {
        console.info('handleDateFilterChanged');
    }

    get datesOptions() {
        const list = this.props.datesFilter.map((date, index) =>
            <option key={index} value={date}>{ date }</option>
        );

        return (
            <select
                id="filter-dates"
                onChange = {this.handleDatesFilterChanged}
            >
                { list }
            </select>
        );
    }

    render() {
        return (
            <DatesFilterView
                dates       = {this.datesOptions}
                datesLength = {_.size(this.props.datesFilter)}
            />
        );
    }
}

DatesFilterContainer.propTypes = {
    datesFilter         : PropTypes.arrayOf(PropTypes.string),
    initFetchDatesFilter: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    datesFilter: getDatesFilter(state),
});

const mapDispatchToProps = dispatch => ({
    initFetchDatesFilter: () => dispatch(fetchDatesFilter()),
});


export default connect(mapStateToProps, mapDispatchToProps)(DatesFilterContainer);
