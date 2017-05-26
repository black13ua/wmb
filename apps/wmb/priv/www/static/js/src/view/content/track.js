import React, { PropTypes } from 'react';
import { Chip, Avatar } from 'react-toolbox';

const TrackView = ({ onPlayClick, onSelectClick, title, selected, active }) =>
    <Chip
        deletable     = {selected}
        style         = {{ display: 'block', margin: '3px', cursor: 'pointer', background: active ? '#FFCA28' : '' }}
        onClick       = {onPlayClick}
        onDeleteClick = {onSelectClick}
    >
        <Avatar
            icon    = {selected ? 'check' : 'add'}
            style   = {{ background: selected ? 'black' : 'grey' }}
            onClick = {onSelectClick}
        />
        <span>{title}</span>
    </Chip>;

TrackView.propTypes = {
    active       : PropTypes.bool,
    selected     : PropTypes.bool.isRequired,
    title        : PropTypes.string.isRequired,
    onPlayClick  : PropTypes.func.isRequired,
    onSelectClick: PropTypes.func.isRequired,
};

export default TrackView;
