import React, { Component } from 'react';
import { AppBar, IconButton } from 'react-toolbox';
import { Layout, NavDrawer, Panel, Sidebar } from 'react-toolbox';
// import PlayerView from '../view/player/player-view';
import isBrowser from 'react-toolbox/lib/utils/is-browser';
import breakpoints from 'react-toolbox/lib/utils/breakpoints';
import { getViewport } from 'react-toolbox/lib/utils/utils';
import RightSidebarContainer from './right-sidebar';
import ContentContainer from './content';
import PlaylistContainer from './left-sidebar';

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

    render() {
        const permanentAt = 'md';
        const appBarIconVisible = this.state.width <= breakpoints[permanentAt];

        return (
            <div style={{ 'margin-top': '100px' }}>
                <AppBar
                    fixed
                    leftIcon={appBarIconVisible ? 'menu' : null}
                    rightIcon={appBarIconVisible ? 'more' : null}
                    onLeftIconClick={this.toggleFiltersPinned}
                    onRightIconClick={this.togglePlaylistPinned}
                />
                <Layout>
                    <NavDrawer active={this.state.drawerActive}
                        pinned={this.state.filtersPinned}
                        permanentAt={permanentAt}
                        onOverlayClick={this.toggleDrawerActive}
                    >
                        <aside style={{ 'margin-top': '100px' }}>
                            <RightSidebarContainer />
                        </aside>
                    </NavDrawer>
                    <Panel>
                        <ContentContainer />
                    </Panel>
                    <Sidebar
                        pinned={this.state.playlistPinned}
                        width={5}
                        permanentAt={permanentAt}
                    >
                        <aside style={{ 'margin-top': '100px' }}>
                            <PlaylistContainer />
                        </aside>
                    </Sidebar>
                </Layout>
            </div>
        );
    }
}


export default App;
