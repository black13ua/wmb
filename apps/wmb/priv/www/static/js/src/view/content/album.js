import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { Media, Image, Button } from 'react-bootstrap';

const AlbumView = ({ onClick, children, album, artist, cover, date, genre, selected }) => {
    // const classNames = classnames({ active: activeClass });

    return (
        <Media>
            <Media.Left>
                <Image width={150} height={150} src={encodeURI(cover)} alt={album} circle />
            </Media.Left>
            <Media.Body>
                <Media.Heading>
                    <span>{ `${artist} | ${album} | ${date} | ${genre}` }</span>
                    <Button
                        bsSize  = "xsmall"
                        bsStyle = {selected ? 'warnign' : 'info'}
                        onClick = {onClick}
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
    // activeClass: PropTypes.bool,
    album   : PropTypes.string.isRequired,
    artist  : PropTypes.string.isRequired,
    children: PropTypes.node,
    cover   : PropTypes.string.isRequired,
    date    : PropTypes.string.isRequired,
    genre   : PropTypes.string.isRequired,
    selected: PropTypes.bool.isRequired,
    onClick : PropTypes.func.isRequired,
};

export default AlbumView;
