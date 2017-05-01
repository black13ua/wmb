import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import CSSModules from 'react-css-modules';
import { badge, listGroup } from 'bootstrap-css';

import CommonFilterView from '../../../../view/right-sidebar/filters/common-filter';
import LetterContainer from './letter';
import { fetchFilter, setFieldValueIO } from '../../../../actions';
import { getFilterDataByAlias, getFilterCurrentValueByAlias } from '../../../../selectors';

const styles = {};
Object.assign(styles, badge, listGroup);


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
            <div styleName="list-group">{ list }</div>
        );
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
    currentValue : getFilterCurrentValueByAlias(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    handleFilterChange: event => dispatch(setFieldValueIO(ownProps.alias, event.target.value)),
});

const styledAbcFilterContainer = CSSModules(AbcFilterContainer, styles, { allowMultiple: true });
export default connect(mapStateToProps, mapDispatchToProps)(styledAbcFilterContainer);
