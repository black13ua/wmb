import React, { Component, PropTypes } from 'react';
import { Chip, Avatar } from 'react-toolbox';

// const hoveredStyle = {
//     display  : 'block',
//     margin   : '4px 6px',
//     cursor   : 'pointer',
//     boxShadow: '3px 3px 5px 0px rgba(0,0,0,0.75)',
//     position : 'relative',
//     right    : '3px',
//     bottom   : '3px',
// };

// const defaultStyle = {
//     display: 'block',
//     margin : '4px 6px',
//     cursor : 'pointer',
// };

class PlaylistTrackView extends Component {
    state = {
        hovered: false,
    };

    handleHover = (value, event) => {
        event.stopPropagation();
        this.setState({ hovered: value });
    }

    render() {
        const { onPlay, onDelete, title, active, album, artist, genre, date, cover } = this.props;
        return (
            <Chip
                onClick = {onPlay}
                deletable
                style         = {{ display: 'block', margin: '4px 6px', cursor: 'pointer', background: active ? '#FFCA28' : '' }}
                onDeleteClick = {onDelete}
                onMouseEnter = {this.handleHover.bind(this, true)}
                onMouseLeave = {this.handleHover.bind(this, false)}
            >
                <Avatar image={encodeURI(cover)} />
                <span>{ `${title} - ${artist}` }</span>
            </Chip>
        );
    }
}

PlaylistTrackView.propTypes = {
    active  : PropTypes.bool.isRequired,
    title   : PropTypes.string.isRequired,
    onDelete: PropTypes.func.isRequired,
    onPlay  : PropTypes.func.isRequired,
};

export default PlaylistTrackView;
