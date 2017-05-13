import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { Media, Image, Button, OverlayTrigger, Tooltip } from 'react-bootstrap';

const AlbumView = ({ onClick, children, album, artist, cover, date, genre, selected, activeClass }) => {
    const classNames = classnames('righted', { active: activeClass });

    const tooltip = (
        <Tooltip id="tooltip">
            <strong>{ `${artist} - ${album} | `}</strong>
            <span>{ `${date} | ${genre}` }</span>
        </Tooltip>
    );

    return (
        <Media>
            <Media.Left>
                <OverlayTrigger
                    overlay   = {tooltip}
                    placement = "top"
                >
                    <Image
                        circle
                        alt    = {album}
                        height = {150}
                        src    = {encodeURI(cover)}
                        width  = {150}
                    />
                </OverlayTrigger>
            </Media.Left>
            <Media.Body>
                <Media.Heading>
                    <span>{ `${artist} - ${album}` }</span>
                    <Button
                        bsSize    = "xsmall"
                        bsStyle   = {selected ? 'warning' : 'info'}
                        className = {classNames}
                        onClick   = {onClick}
                    >
                        { selected ? 'remove' : 'add' }
                    </Button>
                </Media.Heading>
                <Media.Body>
                    { children }
                </Media.Body>
            </Media.Body>
        </Media>
    );
};

AlbumView.propTypes = {
    activeClass: PropTypes.bool,
    album      : PropTypes.string.isRequired,
    artist     : PropTypes.string.isRequired,
    children   : PropTypes.node,
    cover      : PropTypes.string.isRequired,
    date       : PropTypes.string.isRequired,
    genre      : PropTypes.string.isRequired,
    selected   : PropTypes.bool.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default AlbumView;
