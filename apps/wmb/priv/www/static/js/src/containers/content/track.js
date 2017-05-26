import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';

import TrackView from '../../view/content/track';

import { selectTrack, setActiveTrack, playTrack, deletePreviousPlayer } from '../../actions';
import { makeSelectTrackDatabyId } from '../../selectors';

// import debugRender from 'react-render-debugger';
// @debugRender
class TrackContainer extends Component {
    handleSelectAndPlayTrackClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.selectTrack();
        if (!this.props.selected) {
            this.props.setActiveTrack();
            this.props.deletePreviousPlayer();
            this.props.playTrack(this.props.trackData.file);
        }
    }

    handleSelectTrackClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.selectTrack();
    }

    render() {
        const { trackData, selected, active } = this.props;
        if (_.isEmpty(trackData)) return null;

        return (
            <TrackView
                {...trackData}
                active        = {active}
                selected      = {selected}
                onPlayClick   = {this.handleSelectAndPlayTrackClick}
                onSelectClick = {this.handleSelectTrackClick}
            />
        );
    }
}


TrackContainer.propTypes = {
    active              : PropTypes.bool.isRequired,
    deletePreviousPlayer: PropTypes.func.isRequired,
    playTrack           : PropTypes.func.isRequired,
    selected            : PropTypes.bool.isRequired,
    selectTrack         : PropTypes.func.isRequired,
    setActiveTrack      : PropTypes.func.isRequired,
    trackData           : PropTypes.object,
};

const makeMapStateToProps = () => {
    const selectTrackDatabyId = makeSelectTrackDatabyId();

    return createStructuredSelector({
        trackData: selectTrackDatabyId,
    });
};

const mapDispatchToProps = (dispatch, ownProps) => ({
    selectTrack         : ()  => dispatch(selectTrack(ownProps.album, ownProps.trackId, ownProps.selected)),
    setActiveTrack      : ()  => dispatch(setActiveTrack(ownProps.trackId)),
    playTrack           : url => dispatch(playTrack(url)),
    deletePreviousPlayer: ()  => dispatch(deletePreviousPlayer()),
});

export default connect(makeMapStateToProps, mapDispatchToProps)(TrackContainer);
