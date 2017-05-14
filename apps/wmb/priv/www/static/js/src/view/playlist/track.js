import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { Chip, Avatar } from 'react-toolbox';

const PlaylistTrackView = ({ onClick, title, activeClass }) => {
    return (
        <Chip>
          <Avatar style={{ backgroundColor: 'deepskyblue' }} icon="folder" />
          <span>Avatar Chip</span>
        </Chip>
    );
};

PlaylistTrackView.propTypes = {
    activeClass: PropTypes.bool,
    title      : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default PlaylistTrackView;
