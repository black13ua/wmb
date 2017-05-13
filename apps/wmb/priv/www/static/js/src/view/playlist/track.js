import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { ListGroupItem, Button } from 'react-bootstrap';

const PlaylistTrackView = ({ onClick, title, activeClass }) => {
    const classNames = classnames('righted', { active: activeClass });

    return (
        <ListGroupItem>
            <span>{ title }</span>
            <Button
                bsSize  = "xsmall"
                bsStyle = "warning"
                className = {classNames}
                onClick = {onClick}
            >
                {'remove'}
            </Button>
        </ListGroupItem>
    );
};

PlaylistTrackView.propTypes = {
    activeClass: PropTypes.bool,
    title      : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default PlaylistTrackView;
