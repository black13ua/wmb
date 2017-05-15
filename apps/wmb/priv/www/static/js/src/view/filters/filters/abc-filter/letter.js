import React, { PropTypes } from 'react';
// import classnames from 'classnames';
import { ListItem } from 'react-toolbox';

const LetterView = ({ onClick, children, letter, artistCount, activeClass }) => {
    // const classNames = classnames({ active: activeClass });

    return (
        <div>
            <ListItem
                caption = {letter}
                onClick = {onClick}
                rightIcon="keyboard_arrow_right"
            />
            <div>
                { children }
            </div>
        </div>
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
