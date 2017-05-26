import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ProgressBar } from 'react-toolbox';

import FilterItemView from '../../../view/filters/filters/common-filter-item';
import CommonFilterView from '../../../view/filters/filters/common-filter';

import { fetchFilter, setFieldValueIO } from '../../../actions';
import { getFilterDataByAlias, getFilterCurrentValueByAlias, getFetchingState } from '../../../selectors';


// import debugRender from 'react-render-debugger';
// @debugRender
class CommonFilterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    get itemsList() {
        if (this.state.folded) return null;
        if (_.isEmpty(this.props.filterOptions)) {
            return (
                <ProgressBar
                    type='circular'
                    mode='indeterminate'
                    multicolor
                />
            );
        }
        const list = this.props.filterOptions.map((option, index) => {
            const optionId = _.isObject(option) ? option.genreId : option;
            const optionName = _.isObject(option) ? option.genre : option;
            return (
                <FilterItemView
                    active      = {this.props.currentValue === optionName}
                    key         = {index}
                    name        = {optionName}
                    onClick     = {this.handleFilterChange.bind(null, optionId)}
                />
            );
        });
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
        if (_.isEmpty(this.props.filterOptions)) {
            this.props.fetchFilterByAlias();
        }
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
    fetching          : PropTypes.object.isRequired,
};

const mapStateToProps = (state, props) => ({
    filterOptions: getFilterDataByAlias(state, props),
    currentValue : getFilterCurrentValueByAlias(state, props),
    fetching     : getFetchingState(state),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: value => dispatch(setFieldValueIO(ownProps.alias, value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(CommonFilterContainer);
