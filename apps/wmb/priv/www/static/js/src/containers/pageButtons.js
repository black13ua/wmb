import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';
import { Button } from 'react-toolbox';

import { getPages } from '../selectors';
import { fetchAlbumsByPage } from '../actions/index';

class PlaylistContainer extends Component {
    state = {
        activeDialog: false,
    };

    handleClearPlaylist = (value, event) => {
        event.preventDefault();
        event.stopPropagation();
        if (value === this.props.pages.current) return;
        this.props.fetchAlbumsByPage(value);
    }

    render() {
        const { back, previous, current, next, forward } = this.props.pages;
        return (
            <div style = {{ display: 'flex', width: '100%', height: '100%', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'flex-around' }}>
                <Button onClick = {this.handleClearPlaylist.bind(this, back)} icon={back ? 'first_page' : ''} label={back || ''} flat disabled = {!back} />
                <Button onClick = {this.handleClearPlaylist.bind(this, previous)} icon={previous ? 'navigate_before' : ''} label={previous || ''} flat disabled = {!previous} />
                <Button onClick = {this.handleClearPlaylist.bind(this, current)} label={current} flat />
                <Button onClick = {this.handleClearPlaylist.bind(this, next)} icon='navigate_next' label={next} flat />
                <Button onClick = {this.handleClearPlaylist.bind(this, forward)} icon='last_page' label={forward} flat />
            </div>
        );
    }
}

PlaylistContainer.propTypes = {
    pages: PropTypes.object.isRequired,
};

const mapStateToProps = createStructuredSelector({
    pages: getPages,
});

const mapDispatchToProps = dispatch => ({
    fetchAlbumsByPage: currentPage => dispatch(fetchAlbumsByPage(currentPage)),
});

export default connect(mapStateToProps, mapDispatchToProps)(PlaylistContainer);
