import React, { PropTypes } from 'react';
import CSSModules from 'react-css-modules';
import { alert, buttons, jumbotron, normalize } from 'bootstrap-css';

const styles = {};
Object.assign(styles, alert, normalize, jumbotron, buttons);

const LetterView = ({ onClick, children, letter, artistCount }) =>
    /* <li className="filter--item" >
        <b>{ artistCount || '?' }</b>
        <span onClick = {onClick}>{ letter }</span>
        { children }
    </li>;*/

    <div styleName="jumbotron">
        <h1>Hello, world!</h1>
        <a styleName="btn btn-primary btn-lg">Learn more</a>
        <div styleName="alert alert-info">...</div>
    </div>;

LetterView.propTypes = {
    artistCount: PropTypes.number,
    children   : PropTypes.node,
    letter     : PropTypes.string.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default CSSModules(LetterView, styles, { allowMultiple: true });
