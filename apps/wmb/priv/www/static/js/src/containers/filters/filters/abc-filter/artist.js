import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import ArtistView from '../../../../view/filters/filters/abc-filter/artist';
import { fetchAlbumsByArtist } from '../../../../actions';


class ArtistContainer extends Component {
    handleFetchAlbumsByArtist = (event) => {
        event.preventDefault();
        event.stopPropagation();
        if (!this.props.isActive) {
            this.props.fetchAlbumsByArtist();
        }
    }

    render() {
        return (
            <ArtistView
                activeClass = {this.props.isActive}
                artist      = {this.props.artist}
                onClick     = {this.handleFetchAlbumsByArtist}
            />
        );
    }
}


ArtistContainer.propTypes = {
    artist             : PropTypes.string.isRequired,
    artistId           : PropTypes.number.isRequired, // eslint-disable-line
    fetchAlbumsByArtist: PropTypes.func.isRequired,
    isActive           : PropTypes.bool.isRequired,
};

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchAlbumsByArtist: ()    => dispatch(fetchAlbumsByArtist(ownProps.artistId)),
});

export default connect(null, mapDispatchToProps)(ArtistContainer);
