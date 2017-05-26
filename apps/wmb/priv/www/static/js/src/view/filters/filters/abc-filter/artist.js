import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem } from 'react-toolbox';

const ArtistView = ({ onClick, activeClass, artist }) => {
    // const itemClasses = classnames({ active: activeClass });
    return (
        <ListItem
            caption = {artist}
            onClick = {onClick}
        />
    );
};


ArtistView.propTypes = {
    activeClass: PropTypes.bool.isRequired,
    artist     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default ArtistView;
