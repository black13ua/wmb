import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { ListGroupItem } from 'react-bootstrap';

const ArtistView = ({ onClick, activeClass, artist }) => {
    const itemClasses = classnames({ active: activeClass });
    return (
        <ListGroupItem
            className = {itemClasses}
            type      = "button"
            onClick   = {onClick}
        >
            { artist }
        </ListGroupItem>
    );
};


ArtistView.propTypes = {
    activeClass: PropTypes.bool.isRequired,
    artist     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default ArtistView;
