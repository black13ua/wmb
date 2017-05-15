import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import FilterItemView from '../../../view/filters/filters/common-filter-item';
import CommonFilterView from '../../../view/filters/filters/common-filter';

import { fetchFilter, setFieldValueIO } from '../../../actions';
import { getFilterDataByAlias, getFilterCurrentValueByAlias } from '../../../selectors';


class CommonFilterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    componentWillMount() {
        this.props.fetchFilterByAlias();
    }

    get itemsList() {
        if (this.state.folded) return null;
        const list = this.props.filterOptions.map((option, index) =>
            <FilterItemView
                activeClass = {this.props.currentValue === option}
                key         = {index}
                onClick     = {this.handleFilterChange.bind(null, option)}
                name        = { option }
            />
        );
        return <div>{ list }</div>;
    }

    handleFilterChange = (value, event) => {
        event.preventDefault();
        event.stopPropagation();
        if (value === this.props.currentValue) return null;
        this.props.handleFilterChange(value);
    }

    handleSubMenuClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.setState({ folded: !this.state.folded });
    }

    render() {
        return (
            <CommonFilterView
                activeClass   = {!this.state.folded}
                alias         = {this.props.alias}
                optionsLength = {_.size(this.props.filterOptions)}
                onClick       = {this.handleSubMenuClick}
            >
                { this.itemsList }
            </CommonFilterView>
        );
    }
}


CommonFilterContainer.propTypes = {
    alias             : PropTypes.string.isRequired,
    currentValue      : PropTypes.string,
    fetchFilterByAlias: PropTypes.func.isRequired,
    filterOptions     : PropTypes.array,
    handleFilterChange: PropTypes.func.isRequired,
};

const mapStateToProps = (state, props) => ({
    filterOptions: getFilterDataByAlias(state, props),
    currentValue : getFilterCurrentValueByAlias(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: value => dispatch(setFieldValueIO(ownProps.alias, value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(CommonFilterContainer);
