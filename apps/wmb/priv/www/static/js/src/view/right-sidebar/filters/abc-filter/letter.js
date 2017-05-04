import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { ListGroup, ListGroupItem } from 'react-bootstrap';

const LetterView = ({ onClick, children, letter, artistCount, activeClass }) => {
    const classNames = classnames({ active: activeClass });

    return (
        <ListGroup>
            <ListGroupItem
                className={classNames}
                onClick = {onClick}
            >
                { artistCount ? <span className="badge">{`${artistCount}`}</span> : null }
                {`${letter}`}
            </ListGroupItem>
            <div className="second--sub--menu" >
                { children }
            </div>
        </ListGroup>
    );
};

LetterView.propTypes = {
    activeClass: PropTypes.bool,
    artistCount: PropTypes.number,
    children   : PropTypes.node,
    letter     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default LetterView;
