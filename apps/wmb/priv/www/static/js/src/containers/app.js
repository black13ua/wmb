import React, { Component } from 'react';
import { AppBar, Button } from 'react-toolbox';
import { Layout, NavDrawer, Panel, Sidebar } from 'react-toolbox';
// import PlayerView from '../view/player/player-view';
import isBrowser from 'react-toolbox/lib/utils/is-browser';
import breakpoints from 'react-toolbox/lib/utils/breakpoints';
import { getViewport } from 'react-toolbox/lib/utils/utils';
import FiltersContainer from './filters';
import ContentContainer from './content';
import PlaylistContainer from './playlist';

class App extends Component { // eslint-disable-line
    state = {
        filtersPinned : false,
        playlistPinned: false,
        width         : isBrowser() && getViewport().width,
    };

    toggleFiltersPinned = () => {
        this.setState({ filtersPinned: !this.state.filtersPinned });
    }

    togglePlaylistPinned = () => {
        this.setState({ playlistPinned: !this.state.playlistPinned });
    }

    componentDidMount() {
        if (!this.state.width) this.handleResize();
        window.addEventListener('resize', this.handleResize);
    }

    componentWillUnmount() {
        window.removeEventListener('resize', this.handleResize);
    }

    handleResize = () => {
        this.setState({ width: getViewport().width });
    }

    get filterToggleButton() {
        return (
            <Button
                accent
                floating
                icon    = "build"
                style   = {{ position: 'absolute', top: '35px', left: '35px', zIndex: 999 }}
                onClick = {this.toggleFiltersPinned}
            />
        );
    }

    get playlistToggleButton() {
        return (
            <Button
                accent
                floating
                icon    = "assignment"
                style   = {{ position: 'absolute', top: '35px', right: '35px', zIndex: 999 }}
                onClick = {this.togglePlaylistPinned}
            />
        );
    }

    render() {
        const permanentAt = 'md';
        const appBarIconVisible = this.state.width <= breakpoints[permanentAt];

        return (
            <div style={{ marginTop: '100px', position: 'realtive' }}>
                {appBarIconVisible ? this.filterToggleButton : null}
                {appBarIconVisible ? this.playlistToggleButton : null}
                <AppBar fixed />
                <Layout>
                    <NavDrawer
                        active         = {this.state.drawerActive}
                        pinned         = {this.state.filtersPinned}
                        permanentAt    = {permanentAt}
                        width          = "wide"
                        onOverlayClick = {this.toggleDrawerActive}
                    >
                        <FiltersContainer />
                    </NavDrawer>
                    <Panel>
                        <ContentContainer />
                    </Panel>
                    <Sidebar
                        style       = {{ overflow: 'visible' }}
                        pinned      = {this.state.playlistPinned}
                        width       = {5}
                        permanentAt = {permanentAt}
                    >
                        <PlaylistContainer />
                    </Sidebar>
                </Layout>
            </div>
        );
    }
}


export default App;
