import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { Media, Image } from 'react-bootstrap';

const AlbumView = ({ onClick, children, album, artist, cover, date, genre }) => {
    // const classNames = classnames({ active: activeClass });

    return (
        <Media>
            <Media.Left>
                <Image width={64} height={64} src={encodeURI(cover)} alt={album} circle />
            </Media.Left>
            <Media.Body>
                <Media.Heading>
                    { `${artist} | ${album} | ${date} | ${genre}` }
                </Media.Heading>
                <Media.Body>
                    { children }
                </Media.Body>
            </Media.Body>
        </Media>
    );
};

AlbumView.propTypes = {
    // activeClass: PropTypes.bool,
    album   : PropTypes.string.isRequired,
    artist  : PropTypes.string.isRequired,
    children: PropTypes.node,
    cover   : PropTypes.string.isRequired,
    date    : PropTypes.string.isRequired,
    genre   : PropTypes.string.isRequired,
    onClick : PropTypes.func.isRequired,
};

export default AlbumView;
