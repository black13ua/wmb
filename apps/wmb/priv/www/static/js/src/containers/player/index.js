import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { AppBar, ProgressBar, IconButton } from 'react-toolbox';

import {
    getPlayerBuffer,
    getPlayerProgress,
    // getPlayerDuration,
    getPlayerIsPlaying,
} from '../../selectors';
import { toggleTrack, askPlayerProperty } from '../../actions';


// import debugRender from 'react-render-debugger';
// @debugRender
class PlayerContainer extends Component {
    handleToggleClick = (event) => {
        event.stopPropagation();
        event.preventDefault();
        this.props.askIfPlaign();
        this.props.toggleTrack();
    }

    render() {
        return (
            <AppBar fixed style = {{ height: '75px' }} >
                <div style = {{ display: 'flex', width: '100%', margin: '0 20%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                    <IconButton
                        icon  = "skip_previous"
                        style = {{ margin: 'auto 0', color: '#FFEA00' }}
                    />
                    <IconButton
                        icon    = {this.props.playing ? 'play_arrow' : 'pause'}
                        style   = {{ margin: 'auto 0', color: '#FFEA00' }}
                        onClick = {this.handleToggleClick}
                    />
                    <IconButton
                        icon  = "skip_next"
                        style = {{ margin: 'auto 0', color: '#FFEA00' }}
                    />
                </div>
                <div style = {{ width: '100%', position: 'fixed', left: 0, top: '60px' }} >
                    <ProgressBar
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
    // duration   : PropTypes.number,
    playing    : PropTypes.number,
    progress   : PropTypes.number,
    toggleTrack: PropTypes.func.isRequired,
};

const mapStateToProps = createStructuredSelector({
    buffer  : getPlayerBuffer,
    progress: getPlayerProgress,
    // duration: getPlayerDuration,
    playing : getPlayerIsPlaying,
});

const mapDispatchToProps = dispatch => ({
    toggleTrack: () => dispatch(toggleTrack()),
    askIfPlaign: () => dispatch(askPlayerProperty('playing')),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlayerContainer);
