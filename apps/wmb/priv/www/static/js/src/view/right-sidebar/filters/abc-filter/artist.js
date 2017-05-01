import React, { PropTypes } from 'react';
import classnames from 'classnames';


const ArtistView = ({ onClick, activeClass, artist }) => {
    const itemClasses = classnames('filter--item', { active: activeClass });
    return (
        <li className={itemClasses} >
            <span onClick = {onClick}>{ artist }</span>
        </li>
    );
};


ArtistView.propTypes = {
    activeClass: PropTypes.bool.isRequired,
    artist     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default ArtistView;
