import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import ContentView from '../../view/content/content-view';
import AlbumContainer from './album';

import { fetchAlbumsByPage } from '../../actions';
import { getAlbumsIds } from '../../selectors';


class ContentContainer extends Component {
    componentWillMount() {
        this.props.fetchAlbumsByPage();
    }

    get albumsList() {
        const list = this.props.albumsIds.map(id =>
            <AlbumContainer
                id  = {id}
                key = {id}
            />
        );
        return (
            <div>
                { list }
            </div>
        );
    }

    render() {
        return (
            <ContentView>
                { this.albumsList }
            </ContentView>
        );
    }
}


ContentContainer.propTypes = {
    albumsIds        : PropTypes.arrayOf(PropTypes.number),
    fetchAlbumsByPage: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    albumsIds: getAlbumsIds(state),
});

const mapDispatchToProps = dispatch => ({
    fetchAlbumsByPage: () => dispatch(fetchAlbumsByPage()),
});

export default connect(mapStateToProps, mapDispatchToProps)(ContentContainer);
