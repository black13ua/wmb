import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ListGroup, Button } from 'react-bootstrap';
import { createStructuredSelector } from 'reselect';

import PlaylistView from '../../view/playlist/playlist-view';
import PlaylistTrackContainer from './track';
import { getSelectedTrackIds } from '../../selectors';
import { clearPlaylist } from '../../actions/index';

class PlaylistContainer extends Component {
    handleClearPlaylist = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.clearPlaylist();
    }

    get removeButton() {
        const { selectedTrackIds } = this.props;
        if (_.isEmpty(selectedTrackIds)) return null;
        return (
            <Button
                bsSize    = "xsmall"
                bsStyle   = "danger"
                className = "righted"
                onClick   = {this.handleClearPlaylist}
            >
                {'clear'}
            </Button>
        );
    }

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
            <PlaylistView button = {this.removeButton}>
                { this.playlistTracks }
            </PlaylistView>
        );
    }
}

PlaylistContainer.propTypes = {
    clearPlaylist   : PropTypes.func.isRequired,
    selectedTrackIds: PropTypes.arrayOf(PropTypes.number),
};

const mapStateToProps = createStructuredSelector({
    selectedTrackIds: getSelectedTrackIds,
});

const mapDispatchToProps = dispatch => ({
    clearPlaylist: () => dispatch(clearPlaylist()),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlaylistContainer);
