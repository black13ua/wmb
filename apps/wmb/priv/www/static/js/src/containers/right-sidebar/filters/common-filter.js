import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import CommonFilterView from '../../../view/right-sidebar/filters/common-filter';

import { fetchFilter, setFieldValueIO } from '../../../actions';
import { getFilterDataByAlias, getFilterCurrentValueByAlias } from '../../../selectors';


class CommonFilterContainer extends Component {
    componentWillMount() {
        this.props.fetchFilterByAlias();
    }

    get select() {
        const list = this.props.filterOptions.map((option, index) =>
            <option key={index} value={option}>{ option }</option>
        );
        return (
            <select
                id       = {`filter-${this.props.alias}`}
                value    = {this.props.currentValue}
                onChange = {this.props.handleFilterChange}
            >
                <option key={-1} value={'All'}>{ 'All' }</option>
                { list }
            </select>
        );
    }

    render() {
        return (
            <CommonFilterView
                alias         = {this.props.alias}
                optionsLength = {_.size(this.props.filterOptions)}
                select        = {this.select}
            />
        );
    }
}


CommonFilterContainer.propTypes = {
    alias             : PropTypes.string.isRequired,
    currentValue      : PropTypes.string,
    fetchFilterByAlias: PropTypes.func.isRequired,
    filterOptions     : PropTypes.arrayOf(PropTypes.string),
    handleFilterChange: PropTypes.func.isRequired,
};

const mapStateToProps = (state, props) => ({
    filterOptions: getFilterDataByAlias(state, props),
    currentValue : getFilterCurrentValueByAlias(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: event => dispatch(setFieldValueIO(ownProps.alias, event.target.value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(CommonFilterContainer);
