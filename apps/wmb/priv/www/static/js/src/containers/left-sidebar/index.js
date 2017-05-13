import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ListGroup } from 'react-bootstrap';
import { createStructuredSelector } from 'reselect';

import PlaylistView from '../../view/playlist/playlist-view';
import PlaylistTrackContainer from './track';
import { getSelectedTrackIds } from '../../selectors';

class PlaylistContainer extends Component {
    get playlistTracks() {
        const { selectedTrackIds } = this.props;
        if (_.isEmpty(selectedTrackIds)) return null;

        const list = selectedTrackIds.map(trackId =>
            <PlaylistTrackContainer
                key      = {trackId}
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
        return (
            <PlaylistView>
                { this.playlistTracks }
            </PlaylistView>
        );
    }
}

PlaylistContainer.propTypes = {
    selectedTrackIds: PropTypes.arrayOf(PropTypes.number),
};

const mapStateToProps = createStructuredSelector({
    selectedTrackIds: getSelectedTrackIds,
});

export default connect(mapStateToProps)(PlaylistContainer);
