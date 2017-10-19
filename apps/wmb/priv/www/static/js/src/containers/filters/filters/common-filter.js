import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ProgressBar } from 'react-toolbox';
import { includes } from 'lodash';

import FilterItemView from '../../../view/filters/filters/common-filter-item';
import CommonFilterView from '../../../view/filters/filters/common-filter';

import { fetchFilter, fetchAlbumsByFilters, setFieldValue } from '../../../actions';
import { getFilterDataByAlias, getFilterSelectedValuesByAlias, getFetchingState } from '../../../selectors';


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
                    type="circular"
                    mode="indeterminate"
                    multicolor
                />
            );
        }
        const list = this.props.filterOptions.map((option, index) => {
            const optionId = _.isObject(option) ? option.genreId : option;
            const optionName = _.isObject(option) ? option.genre : option;
            return (
                <FilterItemView
                    checked     = {includes(this.props.selectedValues, +optionId)}
                    key         = {index}
                    name        = {optionName}
                    id          = {+optionId}
                    onChange    = {this.handleFilterChange}
                />
            );
        });
        return <div>{ list }</div>;
    }

    handleFilterChange = (id, enable) => {
        this.props.handleFilterChange(this.props.alias, id, enable);
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
    fetchFilterByAlias: PropTypes.func.isRequired,
    fetching          : PropTypes.object.isRequired,
    filterOptions     : PropTypes.array,
    handleFilterChange: PropTypes.func.isRequired,
    selectedValues    : PropTypes.string,
};

const mapStateToProps = (state, props) => ({
    filterOptions : getFilterDataByAlias(state, props),
    selectedValues: getFilterSelectedValuesByAlias(state, props),
    fetching      : getFetchingState(state),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: (alias, value) => {
        dispatch(setFieldValue(alias, value));
        dispatch(fetchAlbumsByFilters());
    },
});

export default connect(mapStateToProps, mapDispatchToProps)(CommonFilterContainer);
