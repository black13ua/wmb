import React, { Component, PropTypes } from 'react';
import { Chip, Avatar } from 'react-toolbox';
import { customEncode } from '../../utils/custom-encode';


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
                deletable
                style         = {{ display: 'block', margin: '4px 6px', cursor: 'pointer', background: active ? '#FFCA28': '' }}
                onClick       = {onPlay}
                onDeleteClick = {onDelete}
                onMouseEnter  = {this.handleHover.bind(this, true)}
                onMouseLeave  = {this.handleHover.bind(this, false)}
            >
                <Avatar image={customEncode(cover)} />
                <span style = {{ color: active ? 'black' : '' }}>{ `${title} - ${artist}` }</span>
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
