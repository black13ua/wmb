import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ProgressBar } from 'react-toolbox';
import ContentView from '../../view/content/content-view';
import AlbumContainer from './album';

import { fetchAlbumsByPage } from '../../actions';
import { getAlbumsIds, getSelectedAlbumIds } from '../../selectors';


class ContentContainer extends Component {
    componentWillMount() {
        this.props.fetchAlbumsByPage();
    }

    get albumsList() {
        const { albumIds, selectedAlbumIds } = this.props;

        const list = albumIds.map(albumId =>
            <AlbumContainer
                albumId  = {albumId}
                key      = {albumId}
                selected = {_.includes(selectedAlbumIds, albumId)}
            />
        );
        return (
            <div>
                { list }
            </div>
        );
    }

    mainSpinner = () =>
        <div style = {{ position: 'fixed', top: '50%', left: '50%' }}>
            <ProgressBar
                type='circular'
                mode='indeterminate'
                multicolor
            />
        </div>;

    render() {
        if (_.isEmpty(this.props.albumIds)) return this.mainSpinner();
        return (
            <ContentView>
                { this.albumsList }
            </ContentView>
        );
    }
}


ContentContainer.propTypes = {
    albumIds         : PropTypes.arrayOf(PropTypes.number),
    fetchAlbumsByPage: PropTypes.func.isRequired,
    selectedAlbumIds : PropTypes.arrayOf(PropTypes.number).isRequired,
};

const mapStateToProps = state => ({
    albumIds        : getAlbumsIds(state),
    selectedAlbumIds: getSelectedAlbumIds(state),
});

const mapDispatchToProps = dispatch => ({
    fetchAlbumsByPage: () => dispatch(fetchAlbumsByPage()),
});

export default connect(mapStateToProps, mapDispatchToProps)(ContentContainer);
