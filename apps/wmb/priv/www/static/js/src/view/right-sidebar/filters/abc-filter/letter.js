import React, { PropTypes } from 'react';
import classnames from 'classnames';


const LetterView = ({ onClick, children, letter, artistCount, activeClass }) => {
    const classNames = classnames('list-group-item', 'button--hover', { active: activeClass });

    return (
        <div styleName="list-group">
            <button
                className={classNames}
                onClick = {onClick}
            >
                <span className="badge">{`${artistCount || '?'}`}</span>
                {`${letter}`}
            </button>
            <div
                className="second--sub--menu"
            >
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
