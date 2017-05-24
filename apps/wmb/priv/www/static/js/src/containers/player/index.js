import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { AppBar, IconButton, CardTitle } from 'react-toolbox';
import YellowProgressBar from '../custom/yellow-progress-bar';
import YellowSlider from '../custom/yellow-slider';
import {
    getPlayerBuffer,
    getPlayerProgress,
    getPlayerIsPlaying,
    getPlayerVolume,
    getActiveTrackData,
} from '../../selectors';
import {
    toggleTrack,
    askPlayerProperty,
    prevTrack,
    nextTrack,
    setPlayerProperty,
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

    onSliderChange = (value) => {
        event.stopPropagation();
        event.preventDefault();
        this.props.volumeChange(value);
        this.props.askVolume();
    }

    get getVolumeIcon() {
        if (this.props.volume === 0) {
            return 'volume_off';
        }
        if (this.props.volume < 50) {
            return 'volume_down';
        }
        return 'volume_up';
    }

    get activeTrackInfo() {
        if (_.isEmpty(this.props.activeTrack)) return;
        const { title, active, album, artist, genre, date, cover } = this.props.activeTrack;
        return (
            <CardTitle
                avatar   = {encodeURI(cover)}
                subtitle = {album}
                title    = {artist}
            />
        );
    }

    render() {
        return (
            <AppBar fixed style = {{ height: '75px' }} >
                <div style = {{ display: 'flex', width: '100%', margin: '0 10%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                    <div style = {{  width: '30%' }}>
                        { this.activeTrackInfo }
                    </div>
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
                    <div style = {{ width: '25%', display: 'flex' }}>
                        <IconButton
                            icon  = {this.getVolumeIcon}
                            style = {{ margin: 'auto 0', color: '#FFEA00' }}
                            onClick = {this.handleMuteClick}
                        />
                        <div style = {{ width: '100%' }}>
                            <YellowSlider
                                pinned
                                max   = {100}
                                min   = {0}
                                value = {this.props.volume}
                                onChange={this.onSliderChange.bind(this)}
                            />
                        </div>
                    </div>
                </div>
                <div style = {{ width: '100%', position: 'fixed', left: 0, top: '60px' }} >
                    <YellowProgressBar
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
    activeTrack : PropTypes.object,
    askIfPlaign : PropTypes.func.isRequired,
    askVolume   : PropTypes.func.isRequired,
    buffer      : PropTypes.number,
    nextTrack   : PropTypes.func.isRequired,
    playing     : PropTypes.number,
    prevTrack   : PropTypes.func.isRequired,
    progress    : PropTypes.number,
    toggleTrack : PropTypes.func.isRequired,
    volume      : PropTypes.number,
    volumeChange: PropTypes.func.isRequired,
};

const mapStateToProps = createStructuredSelector({
    buffer     : getPlayerBuffer,
    progress   : getPlayerProgress,
    playing    : getPlayerIsPlaying,
    volume     : getPlayerVolume,
    activeTrack: getActiveTrackData,
});

const mapDispatchToProps = dispatch => ({
    toggleTrack : () => dispatch(toggleTrack()),
    prevTrack   : () => dispatch(prevTrack()),
    nextTrack   : () => dispatch(nextTrack()),
    askIfPlaign : () => dispatch(askPlayerProperty('playing')),
    askVolume   : () => dispatch(askPlayerProperty('volume')),
    volumeChange: value => dispatch(setPlayerProperty('volume', value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlayerContainer);
