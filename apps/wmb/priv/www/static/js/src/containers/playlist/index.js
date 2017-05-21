import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { IconButton } from 'react-toolbox';

import CleanPlaylistDialog from '../../view/playlist/clean-playlist-dialog';

import PlaylistView from '../../view/playlist/playlist-view';
import PlaylistTrackContainer from './track';
import { getSelectedTrackIds } from '../../selectors';
import { clearPlaylist } from '../../actions/index';


// import debugRender from 'react-render-debugger';
// @debugRender
class PlaylistContainer extends Component {
    state = {
        activeDialog: false,
    };

    handleClearPlaylist = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.setState({ activeDialog: !this.state.activeDialog });
    }

    handleAgreeClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.setState({ activeDialog: !this.state.activeDialog });
        this.props.clearPlaylist();
    }

    actions = [
        { label: 'Cancel', onClick: this.handleClearPlaylist },
        { label: 'Delete', onClick: this.handleAgreeClick },
    ];

    get removeButton() {
        const { selectedTrackIds } = this.props;
        if (_.isEmpty(selectedTrackIds)) return null;
        return (
            <IconButton
                icon    = "delete"
                style   = {{ position: 'absolute', backgroundColor: 'deepskyblue', right: '10px', top: '5px' }}
                onClick = {this.handleClearPlaylist}

            />
        );
    }

    get clearPlaylistDialog() {
        return (
            <CleanPlaylistDialog
                actions       = {this.actions}
                active        = {this.state.activeDialog}
                onCancelClick = {this.handleClearPlaylist}
            />
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
        return list;
    }

    render() {
        return (
            <PlaylistView button = {this.removeButton}>
                { this.playlistTracks }
                { this.clearPlaylistDialog }
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
