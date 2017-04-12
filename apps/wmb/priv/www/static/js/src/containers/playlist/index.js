import React, { Component } from 'react';
import { connect } from 'react-redux';

import PlaylstView from '../../components/playlist/playlist-view';


class PlaylistContainer extends Component {
    render() {
        return (
            <PlaylstView />
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
