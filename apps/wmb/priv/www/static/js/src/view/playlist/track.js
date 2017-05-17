import React, { Component, PropTypes } from 'react';
// import classnames from 'classnames';
import { Chip, Avatar } from 'react-toolbox';

const hoveredStyle = {
    display  : 'block',
    margin   : '3px',
    cursor   : 'pointer',
    transform: 'scale(1.1, 1.1)',
    boxShadow: '4px 4px 5px 0px rgba(0,0,0,0.75)',
    position : 'relative',
    right    : '4px',
    bottom   : '4px',
};

const defaultStyle = {
    display: 'block',
    margin : '3px',
    cursor : 'pointer',
};

class PlaylistTrackView extends Component {
    state = {
        hovered: false,
    };

    handleHover = (value, event) => {
        event.stopPropagation();
        this.setState({ hovered: value });
    }

    render() {
        const { onClick, title, active, album, artist, genre, date, cover } = this.props;
        return (
            <Chip
                deletable
                style         = {this.state.hovered ? hoveredStyle : defaultStyle}
                onDeleteClick = {onClick}
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
    active : PropTypes.bool,
    title  : PropTypes.string.isRequired,
    onClick: PropTypes.func.isRequired,
};

export default PlaylistTrackView;
