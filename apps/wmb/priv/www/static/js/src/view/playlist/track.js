import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { Chip, Avatar } from 'react-toolbox';

const PlaylistTrackView = ({ onClick, title, active, album, artist, genre, date, cover }) => {
    return (
        <Chip
            deletable
            style         = {{ display: 'block', margin: '3px', cursor: 'pointer' }}
            onDeleteClick = {onClick}
        >
            <Avatar
                image={encodeURI(cover)}
            />
            <span>{ `${title} - ${artist}` }</span>
        </Chip>
    );
};

PlaylistTrackView.propTypes = {
    active : PropTypes.bool,
    title  : PropTypes.string.isRequired,
    onClick: PropTypes.func.isRequired,
};

export default PlaylistTrackView;
