import React, { Component } from 'react';

import PlayerView from '../components/player/player-view';
import FiltersContainer from './filters';
import MainContainer from './main';
import PlaylistContainer from './playlist';


class App extends Component { // eslint-disable-line
    render() {
        return (
            <div className = "wrapper--container">
                <PlayerView />
                <FiltersContainer />
                <MainContainer />
                <PlaylistContainer />
            </div>
        );
    }
}


export default App;
