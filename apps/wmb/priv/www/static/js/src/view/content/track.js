import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { Chip, Avatar } from 'react-toolbox';

const TrackView = ({ onClick, title, selected, activeClass, cover }) => {
    const classNames = classnames('righted', { active: activeClass });

    return (
        <Chip
            deletable = {selected}
            style         = {{ display: 'block', margin: '3px', cursor: 'pointer' }}
            onDeleteClick = {onClick}
        >
            <Avatar icon={selected ? 'delete' : 'add'} />
            <span>{title}</span>
        </Chip>
    );
};

TrackView.propTypes = {
    activeClass: PropTypes.bool,
    selected   : PropTypes.bool.isRequired,
    title      : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default TrackView;
