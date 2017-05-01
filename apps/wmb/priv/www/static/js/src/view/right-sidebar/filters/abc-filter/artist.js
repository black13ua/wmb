import React, { PropTypes } from 'react';
import classnames from 'classnames';
import CSSModules from 'react-css-modules';
import { badge, listGroup } from 'bootstrap-css';

const styles = {};
Object.assign(styles, badge, listGroup);

const ArtistView = ({ onClick, activeClass, artist }) => {
    const itemClasses = classnames('list-group-item', { active: activeClass });
    return (
        <button
            type = "button"
            className="button--hover"
            styleName={itemClasses}
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

export default CSSModules(ArtistView, styles, { allowMultiple: true });
