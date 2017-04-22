import React, { Component } from 'react';
import { connect } from 'react-redux';
import Menu, { SubMenu, Item as MenuItem } from 'rc-menu';
import 'rc-menu/assets/index.css';
import classnames from 'classnames';

// import CommonFilterContainer from '../common-filter';
import translate from '../../../../constants/names';
// import AbcFilterContainer from '../abc-filter';
import { fetchFiltersData, setFieldValueIO } from '../../../../actions';
import { getFilters } from '../../../../selectors';


class RcMenuContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            openKeys: [],
        };
    }

    componentWillMount() {
        this.props.fetchFiltersData();
    }

    handleOpenChange = (openKeys) => {
        this.setState({
            openKeys,
        });
    }

    handleMenuClick = (a, b, c) => {
        console.info('handleMenuClick', a, b, c);
    }

    innerItemList() {
        return [1, 2, 3].map((option, index) =>
            <MenuItem key={index} >{ option }</MenuItem>
        );
    }

    itemsList(key) {
        if (key === 'abc') {
            return this.props.filters[key].map(option =>
                <SubMenu
                    key={option}
                    title = {option}
                >
                    { this.innerItemList(option) }
                </SubMenu>
            );
        }
        return this.props.filters[key].map((option, index) =>
            <MenuItem key={index} >{ option }</MenuItem>
        );
    }

    get subMenus() {
        return _.keys(this.props.filters).map((key) => {
            const numberOf = _.size(this.props.filters[key]) || '';
            const subMenuClasses = classnames({ active: _.includes(this.state.openKeys, key) });
            return (
                <SubMenu
                    key   = {key}
                    title = {`${numberOf} ${translate.filterHeaders[key] || key}`}
                    className = {subMenuClasses}
                >
                    { this.itemsList(key) }
                </SubMenu>
            );
        });
    }

    render() {
        return (
            <Menu
                mode         = "inline"
                openKeys     = {this.state.openKeys}
                onOpenChange = {this.handleOpenChange}
            >
                { this.subMenus }
            </Menu>
        );
    }
}


const mapStateToProps = state => ({
    filters: getFilters(state),
    // currentValue  : getFiltersCurrentValue(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchFiltersData  : ()    => dispatch(fetchFiltersData()),
    handleFilterChange: event => dispatch(setFieldValueIO(ownProps.alias, event.target.value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(RcMenuContainer);
