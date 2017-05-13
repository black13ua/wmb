import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListGroupItem, Button } from 'react-bootstrap';

const TrackView = ({ onClick, title, selected }) => {
    // const classNames = classnames({ active: activeClass });

    return (
        <ListGroupItem>
            <span>{ title }</span>
            <Button
                bsSize  = "xsmall"
                bsStyle = {selected ? 'warnign': 'info'}
                onClick = {onClick}
            >
                { selected ? 'remove' : 'add' }
            </Button>
        </ListGroupItem>
    );
};

TrackView.propTypes = {
    // activeClass: PropTypes.bool,
    title  : PropTypes.string.isRequired,
    onClick: PropTypes.func.isRequired,
};

export default TrackView;
