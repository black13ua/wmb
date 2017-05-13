import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { ListGroup } from 'react-bootstrap';

import AlbumView from '../../view/content/album';
import TrackContainer from './track';

import { selectAlbum } from '../../actions';
import { makeSelectAlbumDatabyId, getSelectedTrackIds } from '../../selectors';


class AlbumContainer extends Component {
    handleSelectAlbumClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.selectAlbum();
    }

    get trackList() {
        const { albumId, albumData, selectedTrackIds } = this.props;
        if (_.isEmpty(albumData.trackIds)) return null;

        const list = albumData.trackIds.map(trackId =>
            <TrackContainer
                albumId  = {albumId}
                key      = {trackId}
                selected = {_.includes(selectedTrackIds, trackId)}
                trackId  = {trackId}
            />
        );
        return (
            <ListGroup>
                { list }
            </ListGroup>
        );
    }

    render() {
        const { albumData, selected } = this.props;
        if (_.isEmpty(albumData)) return null;

        return (
            <AlbumView
                {...albumData}
                selected = {selected}
                onClick  = {this.handleSelectAlbumClick}
            >
                { this.trackList }
            </AlbumView>
        );
    }
}


AlbumContainer.propTypes = {
    albumData       : PropTypes.object,
    albumId         : PropTypes.number.isRequired,
    selectAlbum     : PropTypes.func.isRequired,
    selected        : PropTypes.bool.isRequired,
    selectedTrackIds: PropTypes.arrayOf(PropTypes.number),
};

const makeMapStateToProps = () => {
    const selectAlbumDatabyId = makeSelectAlbumDatabyId();

    return createStructuredSelector({
        albumData       : selectAlbumDatabyId,
        selectedTrackIds: getSelectedTrackIds,
    });
};

const mapDispatchToProps = (dispatch, ownProps) => ({
    selectAlbum: () => dispatch(selectAlbum(ownProps.albumId, ownProps.selected)),
});

export default connect(makeMapStateToProps, mapDispatchToProps)(AlbumContainer);
