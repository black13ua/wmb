import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { AppBar, ProgressBar, IconButton } from 'react-toolbox';

import {
    getPlayerBuffer,
    getPlayerProgress,
    getPlayerIsPlaying,
} from '../../selectors';
import {
    toggleTrack,
    askPlayerProperty,
    prevTrack,
    nextTrack,
} from '../../actions';


// import debugRender from 'react-render-debugger';
// @debugRender
class PlayerContainer extends Component {
    handleToggleClick = (event) => {
        event.stopPropagation();
        event.preventDefault();
        this.props.toggleTrack();
        this.props.askIfPlaign();
    }

    handlePrevTrackClick = (event) => {
        event.stopPropagation();
        event.preventDefault();
        this.props.prevTrack();
    }

    handleNextTrackClick = (event) => {
        event.stopPropagation();
        event.preventDefault();
        this.props.nextTrack();
    }

    render() {
        return (
            <AppBar fixed style = {{ height: '75px' }} >
                <div style = {{ display: 'flex', width: '100%', margin: '0 20%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                    <IconButton
                        icon  = "skip_previous"
                        style = {{ margin: 'auto 0', color: '#FFEA00' }}
                        onClick = {this.handlePrevTrackClick}
                    />
                    <IconButton
                        icon    = {this.props.playing ? 'pause' : 'play_arrow'}
                        style   = {{ margin: 'auto 0', color: '#FFEA00' }}
                        onClick = {this.handleToggleClick}
                    />
                    <IconButton
                        icon  = "skip_next"
                        style = {{ margin: 'auto 0', color: '#FFEA00' }}
                        onClick = {this.handleNextTrackClick}
                    />
                </div>
                <div style = {{ width: '100%', position: 'fixed', left: 0, top: '60px' }} >
                    <ProgressBar
                        multicolor
                        buffer = {this.props.buffer}
                        mode   = "determinate"
                        value  = {this.props.progress}
                    />
                </div>
            </AppBar>
        );
    }
}

PlayerContainer.propTypes = {
    askIfPlaign: PropTypes.func.isRequired,
    buffer     : PropTypes.number,
    nextTrack  : PropTypes.func.isRequired,
    playing    : PropTypes.number,
    prevTrack  : PropTypes.func.isRequired,
    progress   : PropTypes.number,
    toggleTrack: PropTypes.func.isRequired,
};

const mapStateToProps = createStructuredSelector({
    buffer  : getPlayerBuffer,
    progress: getPlayerProgress,
    playing : getPlayerIsPlaying,
});

const mapDispatchToProps = dispatch => ({
    toggleTrack: () => dispatch(toggleTrack()),
    prevTrack  : () => dispatch(prevTrack()),
    nextTrack  : () => dispatch(nextTrack()),
    askIfPlaign: () => dispatch(askPlayerProperty('playing')),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlayerContainer);
