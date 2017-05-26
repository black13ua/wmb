import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { IconButton } from 'react-toolbox';

import CleanPlaylistDialog from '../../view/playlist/clean-playlist-dialog';

import PlaylistView from '../../view/playlist/playlist-view';
import PlaylistTrackContainer from './track';
import { getSelectedTrackIds, getRepeatPlaylistStatus, getActiveTrackId } from '../../selectors';
import { clearPlaylist, shufflePlaylist, repeatPlaylist } from '../../actions/index';


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

    handleShufflePlaylist = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.shufflePlaylist();
    }

    handleRepeatPlaylist = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.repeatPlaylist();
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
        const { selectedTrackIds, repeat } = this.props;
        if (_.isEmpty(selectedTrackIds)) return null;
        return (
            <div style = {{ display: 'flex', width: '100%', flexWrap: 'wrap', justifyContent: 'flex-end', alignContent: 'flex-around' }} >
                <IconButton
                    icon    = "cached"
                    style   = {{ backgroundColor: repeat ? '#FFCA28' : 'grey', margin: '0 5px' }}
                    onClick = {this.handleRepeatPlaylist}
                />
                <IconButton
                    icon    = "shuffle"
                    style   = {{ backgroundColor: '#FFCA28', margin: '0 5px' }}
                    onClick = {this.handleShufflePlaylist}
                />
                <IconButton
                    icon    = "clear"
                    style   = {{ backgroundColor: '#FFCA28', margin: '0 5px' }}
                    onClick = {this.handleClearPlaylist}
                />
            </div>
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
        const { selectedTrackIds, activeTrackId } = this.props;
        if (_.isEmpty(selectedTrackIds)) return null;

        const list = selectedTrackIds.map(trackId =>
            <PlaylistTrackContainer
                active   = {trackId === activeTrackId}
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
    activeTrackId   : PropTypes.number.isRequired,
    clearPlaylist   : PropTypes.func.isRequired,
    repeat          : PropTypes.bool.isRequired,
    repeatPlaylist  : PropTypes.func.isRequired,
    selectedTrackIds: PropTypes.arrayOf(PropTypes.number),
    shufflePlaylist : PropTypes.func.isRequired,
};

const mapStateToProps = createStructuredSelector({
    selectedTrackIds: getSelectedTrackIds,
    repeat          : getRepeatPlaylistStatus,
    activeTrackId   : getActiveTrackId,
});

const mapDispatchToProps = dispatch => ({
    clearPlaylist  : () => dispatch(clearPlaylist()),
    shufflePlaylist: () => dispatch(shufflePlaylist()),
    repeatPlaylist : () => dispatch(repeatPlaylist()),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlaylistContainer);
