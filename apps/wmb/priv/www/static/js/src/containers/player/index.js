import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { AppBar, IconButton, CardTitle } from 'react-toolbox';
import ProgressBar from './progress-bar';
import YellowSlider from '../custom/yellow-slider';
import MenuTest from './active-track-menu';

import { setVolumeToLocalStorage } from '../../utils/local-storage';

import {
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
    setStoreProperty,
} from '../../actions';


// import debugRender from 'react-render-debugger';
// @debugRender
class PlayerContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            previousVolume: 0,
        };
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
        if (_.isEmpty(this.props.activeTrack)) return null;
        const { title, artist, cover } = this.props.activeTrack;
        return (
            <CardTitle
                avatar   = {encodeURI(cover)}
                subtitle = {artist}
                title    = {title}
            />
        );
    }

    get menuButton() {
        if (_.isEmpty(this.props.activeTrack)) return null;
        const { activeTrack } = this.props;
        return (
            <MenuTest
                {...activeTrack}
                active = {false}
            />
        );
    }

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

    handleToggleMuteClick = (event) => {
        event.stopPropagation();
        event.preventDefault();
        const volume = this.props.volume ? 0 : this.state.previousVolume;
        this.setState({ previousVolume: this.props.volume });
        if (this.props.playing) {
            this.props.volumeChange(volume);
            this.props.askVolume();
        } else {
            this.props.setStoreVolume(volume);
        }
    }

    handleSliderChange = (value) => {
        event.stopPropagation();
        event.preventDefault();
        if (this.props.playing) {
            this.props.volumeChange(value);
            this.props.askVolume();
        } else {
            this.props.setStoreVolume(value);
        }
    }

    render() {
        return (
            <AppBar fixed style = {{ height: '75px' }} >
                <div style = {{ display: 'flex', width: '100%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                    <div style = {{ width: '30%', display: 'flex' }}>
                        { this.activeTrackInfo }
                        {/* { this.menuButton }*/}
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
                    <div style = {{ width: '15%', display: 'flex' }}>
                        <IconButton
                            icon  = {this.getVolumeIcon}
                            style = {{ margin: 'auto 0', color: '#FFEA00' }}
                            onClick = {this.handleToggleMuteClick}
                        />
                        <div style = {{ width: '100%', margin: 'auto' }}>
                            <YellowSlider
                                pinned
                                max   = {100}
                                min   = {0}
                                value = {this.props.volume}
                                onChange={this.handleSliderChange.bind(this)}
                            />
                        </div>
                    </div>
                </div>
                <ProgressBar />
            </AppBar>
        );
    }
}

PlayerContainer.propTypes = {
    activeTrack   : PropTypes.object,
    askIfPlaign   : PropTypes.func.isRequired,
    askVolume     : PropTypes.func.isRequired,
    nextTrack     : PropTypes.func.isRequired,
    playing       : PropTypes.bool,
    prevTrack     : PropTypes.func.isRequired,
    setStoreVolume: PropTypes.func.isRequired,
    toggleTrack   : PropTypes.func.isRequired,
    volume        : PropTypes.number,
    volumeChange  : PropTypes.func.isRequired,
};

const mapStateToProps = createStructuredSelector({
    playing    : getPlayerIsPlaying,
    volume     : getPlayerVolume,
    activeTrack: getActiveTrackData,
});

const mapDispatchToProps = dispatch => ({
    toggleTrack   : ()    => dispatch(toggleTrack()),
    prevTrack     : ()    => dispatch(prevTrack()),
    nextTrack     : ()    => dispatch(nextTrack()),
    askIfPlaign   : ()    => dispatch(askPlayerProperty('playing')),
    askVolume     : ()    => dispatch(askPlayerProperty('volume')),
    setStoreVolume: value => dispatch(setStoreProperty('volume', value)),
    volumeChange  : value => dispatch(setPlayerProperty('volume', value)),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlayerContainer);
