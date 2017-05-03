import React, { PropTypes } from 'react';
import classnames from 'classnames';


const ArtistView = ({ onClick, activeClass, artist }) => {
    const itemClasses = classnames('list-group-item', 'button--hover', { active: activeClass });
    return (
        <button
            className = {itemClasses}
            type      = "button"
        >
            <span onClick = {onClick}>{ artist }</span>
        </button>
    );
};


ArtistView.propTypes = {
    activeClass: PropTypes.bool.isRequired,
    artist     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default ArtistView;
