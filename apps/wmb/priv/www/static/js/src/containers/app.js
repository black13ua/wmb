import React, { Component } from 'react';
import { Grid, Row } from 'react-bootstrap';

import PlayerView from '../view/player/player-view';
import RightSidebarContainer from './right-sidebar';
import ContentContainer from './content';
import PlaylistContainer from './left-sidebar';


class App extends Component { // eslint-disable-line
    render() {
        return (
            <Grid>
                <PlayerView />
                <Row>
                    <RightSidebarContainer />
                    <ContentContainer />
                    <PlaylistContainer />
                </Row>
            </Grid>
        );
    }
}


export default App;
