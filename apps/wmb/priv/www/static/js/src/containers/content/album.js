import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import TrackContainer from './track';

import { fetchFilter, setFieldValueIO } from '../../../actions';
import { getFilterDataByAlias, getFilterCurrentValueByAlias } from '../../../selectors';


class AlbumContainer extends Component {



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


AlbumContainer.propTypes = {
    id           : PropTypes.number.isRequired,
    albumDatabyId: PropTypes.string,
};

const mapStateToProps = (state, props) => ({
    albumDatabyId: getAlbumDataById(state, props),
});

const mapDispatchToProps = (dispatch) => ({
    // fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
    // handleFilterChange: event => dispatch(setFieldValueIO(ownProps.alias, event.target.value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(AlbumContainer);
