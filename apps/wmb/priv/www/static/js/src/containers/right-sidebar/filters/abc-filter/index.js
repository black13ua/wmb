import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import AbcFilterView from '../../../../view/right-sidebar/filters/abc-filter';
import LetterContainer from './letter';
import { fetchFilter, setFieldValueIO } from '../../../../actions';
import { getFilterDataByAlias, getFilterCurrentValueByAlias } from '../../../../selectors';


class AbcFilterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    componentWillMount() {
        this.props.fetchFilterByAlias();
    }

    handleUnfoldList = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.setState({ folded: !this.state.folded });
    }

    get abcList() {
        if (this.state.folded) return null;

        const list = this.props.filterOptions.map(letterObj =>
            <LetterContainer
                key      = {letterObj.letterId}
                letter   = {letterObj.letter}
                letterId = {letterObj.letterId}
            />
        );
        return (
            <ul>
                { list }
            </ul>
        );
    }

    render() {
        return (
            <AbcFilterView
                alias         = {this.props.alias}
                optionsLength = {_.size(this.props.filterOptions)}
                onClick       = {this.handleUnfoldList}
            >
                {this.abcList}
            </AbcFilterView>
        );
    }
}


AbcFilterContainer.propTypes = {
    alias             : PropTypes.string.isRequired,
    // currentValue      : PropTypes.string,
    fetchFilterByAlias: PropTypes.func.isRequired,
    filterOptions     : PropTypes.array,
    // handleFilterChange: PropTypes.func.isRequired,
};

const mapStateToProps = (state, props) => ({
    filterOptions: getFilterDataByAlias(state, props),
    currentValue : getFilterCurrentValueByAlias(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: event => dispatch(setFieldValueIO(ownProps.alias, event.target.value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(AbcFilterContainer);
