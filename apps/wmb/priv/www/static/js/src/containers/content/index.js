import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import ContentView from '../../view/content/content-view';
import AlbumContainer from './album';

import { fetchDataByPage } from '../../actions';
import { getAlbumsIds } from '../../selectors';


class ContentContainer extends Component {
    componentWillMount() {
        this.props.fetchDataByPage();
    }

    get albumsList() {
        const list = this.props.albumsIds.map(id =>
            <AlbumContainer
                id  = {id}
                key = {id}
            />
        );
        return (
            <ul className="main-container">
                { list }
            </ul>
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
    albumsIds      : PropTypes.arrayOf(PropTypes.number),
    fetchDataByPage: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    albumsIds: getAlbumsIds(state),
});

const mapDispatchToProps = dispatch => ({
    fetchDataByPage: () => dispatch(fetchDataByPage()),
});

export default connect(mapStateToProps, mapDispatchToProps)(ContentContainer);
