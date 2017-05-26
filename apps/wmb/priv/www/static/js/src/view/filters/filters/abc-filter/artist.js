import React, { PropTypes } from 'react';
import { ListItem } from 'react-toolbox';

const ArtistView = ({ onClick, artist }) =>
    <ListItem
        caption = {artist}
        onClick = {onClick}
    />;


ArtistView.propTypes = {
    artist : PropTypes.string.isRequired,
    onClick: PropTypes.func.isRequired,
};

export default ArtistView;
