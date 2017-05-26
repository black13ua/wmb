import React, { Component } from 'react';
import { Button, Layout, NavDrawer, Panel, Sidebar } from 'react-toolbox';
import isBrowser from 'react-toolbox/lib/utils/is-browser';
import breakpoints from 'react-toolbox/lib/utils/breakpoints';
import { getViewport } from 'react-toolbox/lib/utils/utils';
import ScrollUp from 'react-scroll-up';
import PlayerContainer from './player';
import GreyAppBar from './custom/grey-appbar';
import FiltersContainer from './filters';
import ContentContainer from './content';
import PlaylistContainer from './playlist';
import PageButtonsContainer from './pageButtons';


// import debugRender from 'react-render-debugger';
// @debugRender
class App extends Component { // eslint-disable-line
    state = {
        filtersPinned : false,
        playlistPinned: false,
        paused        : true,
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
                style   = {{ position: 'fixed', top: '35px', left: '35px', zIndex: 999, color: '#424242' }}
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
                style   = {{ position: 'fixed', top: '35px', right: '35px', zIndex: 999, color: '#424242' }}
                onClick = {this.togglePlaylistPinned}
            />
        );
    }

    toTopButton = () =>
        <ScrollUp
            duration  = {1000}
            easing    = "easeOutCubic"
            showUnder = {350}
            style     = {{ position: 'fixed', bottom: '65px', right: '35px', zIndex: 999, transitionDuration: '0.5s' }}
        >
            <Button
                accent
                floating
                icon  = "arrow_upward"
                style = {{ color: '#424242' }}
            />
        </ScrollUp>;

    render() {
        const permanentAt = 'md';
        const appBarIconVisible = this.state.width <= breakpoints[permanentAt];

        return (
            <div style={{ margin: '70px 0 30px', position: 'realtive' }} >
                {appBarIconVisible ? this.filterToggleButton : null}
                {appBarIconVisible ? this.playlistToggleButton : null}
                <PlayerContainer />
                <Layout>
                    <NavDrawer
                        active         = {this.state.drawerActive}
                        permanentAt    = {permanentAt}
                        pinned         = {this.state.filtersPinned}
                        width          = "wide"
                        onOverlayClick = {this.toggleDrawerActive}
                    >
                        <FiltersContainer />
                    </NavDrawer>
                    <Panel>
                        <ContentContainer />
                    </Panel>
                    <Sidebar
                        permanentAt = {permanentAt}
                        pinned      = {this.state.playlistPinned}
                        style       = {{ overflow: 'visible' }}
                        width       = {5}
                    >
                        <PlaylistContainer />
                    </Sidebar>
                </Layout>
                <div style = {{ position: 'fixed', bottom: 0, width: '100%', zIndex: 200, height: '35px' }}>
                    <GreyAppBar >
                        <PageButtonsContainer />
                    </GreyAppBar>
                </div>
                { this.toTopButton() }
            </div>
        );
    }
}

export default App;
