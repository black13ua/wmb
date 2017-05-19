import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { ProgressBar, Snackbar } from 'react-toolbox';
import ContentView from '../../view/content/content-view';
import AlbumContainer from './album';

import { fetchAlbumsByPage, clearWarningState } from '../../actions';
import { getAlbumsIds, getSelectedAlbumIds, getWarningMessage, getPages } from '../../selectors';


class ContentContainer extends Component {
    componentWillMount() {
        this.props.fetchAlbumsByPage(this.props.pages.current);
    }

    handleSnackbarTimeout = () => {
        this.props.clearWarningState();
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
        return list;
    }

    mainSpinner = () =>
        <div style = {{ position: 'fixed', top: '50%', left: '50%' }}>
            <ProgressBar
                type='circular'
                mode='indeterminate'
                multicolor
            />
        </div>;

    get warningMessage() {
        return (
            <Snackbar
                action    = 'Dismiss'
                active    = {this.props.warning}
                label     = {this.props.warning}
                timeout   = {3000}
                onClick   = {this.handleSnackbarTimeout}
                onTimeout = {this.handleSnackbarTimeout}
                type='warning'
            />
        );
    }

    render() {
        if (_.isEmpty(this.props.albumIds)) return this.mainSpinner();
        return (
            <ContentView>
                { this.albumsList }
                { this.warningMessage }
            </ContentView>
        );
    }
}


ContentContainer.propTypes = {
    albumIds         : PropTypes.arrayOf(PropTypes.number),
    fetchAlbumsByPage: PropTypes.func.isRequired,
    selectedAlbumIds : PropTypes.arrayOf(PropTypes.number).isRequired,
    warning          : PropTypes.string,
};

const mapStateToProps = state => ({
    albumIds        : getAlbumsIds(state),
    selectedAlbumIds: getSelectedAlbumIds(state),
    warning         : getWarningMessage(state),
    pages           : getPages(state),
});

const mapDispatchToProps = dispatch => ({
    fetchAlbumsByPage: currentPage => dispatch(fetchAlbumsByPage(currentPage)),
    clearWarningState: ()          => dispatch(clearWarningState()),
});

export default connect(mapStateToProps, mapDispatchToProps)(ContentContainer);
