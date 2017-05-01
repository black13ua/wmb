import React, { PropTypes } from 'react';
import CSSModules from 'react-css-modules';
import { badge, listGroup } from 'bootstrap-css';

const styles = {};
Object.assign(styles, badge, listGroup);

const LetterView = ({ onClick, children, letter, artistCount }) =>
    <div styleName="list-group">
        <button
            type = "button"
            className="button--hover"
            styleName="list-group-item"
            onClick = {onClick}
        >
            <span styleName="badge">{`${artistCount || '?'}`}</span>
            {`${letter}`}
        </button>
        <div
            className="second--sub--menu"
        >
            { children }
        </div>
    </div>;

LetterView.propTypes = {
    artistCount: PropTypes.number,
    children   : PropTypes.node,
    letter     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default CSSModules(LetterView, styles, { allowMultiple: true });
