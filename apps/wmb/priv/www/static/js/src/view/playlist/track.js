import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { Chip, Avatar } from 'react-toolbox';

const PlaylistTrackView = ({ onClick, title, activeClass }) => {
    return (
        <Chip
            deletable
            style         = {{ display: 'block', margin: '3px', cursor: 'pointer' }}
            onDeleteClick = {onClick}
        >
            <Avatar
                style = {{ backgroundColor: activeClass ? 'deepskyblue' : 'deeppink' }}
                icon  = {activeClass ? 'play_arrow' : 'music_note'}
            />
            <span>{ title }</span>
        </Chip>
    );
};

PlaylistTrackView.propTypes = {
    activeClass: PropTypes.bool,
    title      : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default PlaylistTrackView;
