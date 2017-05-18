import React, { Component } from 'react';
import { AppBar, Button } from 'react-toolbox';
import { Layout, NavDrawer, Panel, Sidebar, IconButton } from 'react-toolbox';
import isBrowser from 'react-toolbox/lib/utils/is-browser';
import breakpoints from 'react-toolbox/lib/utils/breakpoints';
import { getViewport } from 'react-toolbox/lib/utils/utils';
import GreyAppBar from './greyAppBar';
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
                style   = {{ position: 'fixed', top: '35px', left: '35px', zIndex: 999 }}
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
                style   = {{ position: 'fixed', top: '35px', right: '35px', zIndex: 999 }}
                onClick = {this.togglePlaylistPinned}
            />
        );
    }

    render() {
        const permanentAt = 'md';
        const appBarIconVisible = this.state.width <= breakpoints[permanentAt];

        return (
            <div style={{ marginTop: '70px', position: 'realtive' }} >
                {appBarIconVisible ? this.filterToggleButton : null}
                {appBarIconVisible ? this.playlistToggleButton : null}
                <AppBar scrollHide fixed style = {{ height: '75px' }} >
                    <div style = {{ display: 'flex', width: '100%', margin: '0 20%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                        <IconButton icon='skip_previous' floating mini style = {{ margin: 'auto 0', color: 'wheat' }} />
                        <Button icon='play_arrow' floating accent style = {{ margin: 'auto 0', color: 'wheat' }} />
                        <IconButton icon='skip_next' floating mini style = {{ margin: 'auto 0', color: 'wheat' }} />
                    </div>
                </AppBar>
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
                <div style = {{ position: 'fixed', bottom: 0, width: '100%', zIndex: 200, height: '35px' }}>
                    <GreyAppBar >
                        <div style = {{ display: 'flex', width: '100%', height: '100%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                            <Button icon='first_page' label='10' flat />
                            <Button icon='navigate_before' label='12' flat />
                            <Button label='13' flat />
                            <Button icon='navigate_next' label='14' flat />
                            <Button icon='last_page' label='20' flat />
                        </div>
                    </GreyAppBar>
                </div>
            </div>
        );
    }
}


export default App;
