import React, { Component } from 'react';
import { Grid, Row, Pagination } from 'react-bootstrap';

import PlayerView from '../view/player/player-view';
import RightSidebarContainer from './right-sidebar';
import ContentContainer from './content';
import PlaylistContainer from './left-sidebar';


class App extends Component { // eslint-disable-line
    render() {
        return (
            <Grid>
                <Row>
                    <PlayerView />
                </Row>
                <Row>
                    <RightSidebarContainer />
                    <ContentContainer />
                    <PlaylistContainer />
                </Row>
                <Row>
                    <Pagination
                        prev
                        next
                        first
                        last
                        ellipsis
                        boundaryLinks
                        items={20}
                        maxButtons={5}
                    />
                </Row>
            </Grid>
        );
    }
}


export default App;
