import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { ListGroupItem, Button } from 'react-bootstrap';

const TrackView = ({ onClick, title, selected, activeClass }) => {
    const classNames = classnames('righted', { active: activeClass });

    return (
        <ListGroupItem>
            <span>{ title }</span>
            <Button
                bsSize    = "xsmall"
                bsStyle   = {selected ? 'warning' : 'info'}
                className = {classNames}
                onClick   = {onClick}
            >
                { selected ? 'remove' : 'add' }
            </Button>
        </ListGroupItem>
    );
};

TrackView.propTypes = {
    activeClass: PropTypes.bool,
    selected   : PropTypes.bool.isRequired,
    title      : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default TrackView;
