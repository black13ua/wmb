import React, { Component } from 'react';
import { connect } from 'react-redux';

import PlaylistView from '../../view/playlist/playlist-view';


class PlaylistContainer extends Component {
    render() {
        return (
            <PlaylistView />
        );
    }
}

PlaylistContainer.propTypes = {

};

const mapStateToProps = () => ({
});

const mapDispatchToProps = () => ({
});


export default connect(mapStateToProps, mapDispatchToProps())(PlaylistContainer);
