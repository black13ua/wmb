import React, { PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';

import AlbumView from '../../view/content/album';

// import { fetchFilter, setFieldValueIO } from '../../../actions';
import { makeSelectAlbumDatabyId } from '../../selectors';


const AlbumContainer = ({ albumData }) =>
    <AlbumView
        key = {albumData.albumId}
        {...albumData}
    />;


AlbumContainer.propTypes = {
    albumData: PropTypes.object,
};

const makeMapStateToProps = () => {
    const selectAlbumDatabyId = makeSelectAlbumDatabyId();

    return createStructuredSelector({
        albumData: selectAlbumDatabyId,
    });
};

// const mapDispatchToProps = (dispatch) => ({
//     fetchFilterByAlias: ()    => dispatch(fetchFilter(ownProps.alias)),
//     handleFilterChange: event => dispatch(setFieldValueIO(ownProps.alias, event.target.value)),
// });

export default connect(makeMapStateToProps)(AlbumContainer);
