import React, { Component } from 'react';

import PlayerView from '../view/player/player-view';
import RightSidebarContainer from './right-sidebar';
import MainContainer from './main';
import PlaylistContainer from './left-sidebar';


class App extends Component { // eslint-disable-line
    render() {
        return (
            <div className = "wrapper--container">
                <PlayerView />
                <RightSidebarContainer />
                <MainContainer />
                <PlaylistContainer />
            </div>
        );
    }
}


export default App;
