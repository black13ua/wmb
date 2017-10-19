import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ProgressBar } from 'react-toolbox';

import CommonFilterView from '../../../../view/filters/filters/common-filter';
import LetterContainer from './letter';
import { fetchFilter, fetchAlbumsByFilters } from '../../../../actions';
import { getFilterDataByAlias, getFilterSelectedValuesByAlias } from '../../../../selectors';

// import debugRender from 'react-render-debugger';
// @debugRender
class AbcFilterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    handleUnfoldList = (event) => {
        event.preventDefault();
        event.stopPropagation();
        if (_.isEmpty(this.props.filterOptions)) {
            this.props.fetchFilterByAlias();
        }
        this.setState({ folded: !this.state.folded });
    }

    get abcList() {
        if (this.state.folded) return null;
        if (_.isEmpty(this.props.filterOptions)) {
            return (
                <ProgressBar
                    multicolor
                    mode = "indeterminate"
                    type = "circular"
                />
            );
        }
        const list = _(this.props.filterOptions)
            .sortBy('letter')
            .map(letterObj =>
                <LetterContainer
                    key      = {letterObj.letterId}
                    letter   = {letterObj.letter}
                    letterId = {letterObj.letterId}
                />
            )
            .value();

        return <div>{ list }</div>;
    }

    render() {
        return (
            <CommonFilterView
                activeClass   = {!this.state.folded}
                alias         = {this.props.alias}
                optionsLength = {_.size(this.props.filterOptions)}
                onClick       = {this.handleUnfoldList}
            >
                {this.abcList}
            </CommonFilterView>
        );
    }
}


AbcFilterContainer.propTypes = {
    alias             : PropTypes.string.isRequired,
    fetchFilterByAlias: PropTypes.func.isRequired,
    filterOptions     : PropTypes.array,
};

const mapStateToProps = (state, props) => ({
    filterOptions: getFilterDataByAlias(state, props),
    currentValue : getFilterSelectedValuesByAlias(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: event => dispatch(fetchAlbumsByFilters(ownProps.alias, event.target.value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(AbcFilterContainer);
