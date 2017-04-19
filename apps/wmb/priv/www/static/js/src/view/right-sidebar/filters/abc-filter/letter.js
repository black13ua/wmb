import React, { PropTypes } from 'react';

const LetterView = ({ onClick, children, letter }) =>
    <div className="letter" >
        {/* <b>{ optionsLength }</b>*/}
        <button onClick = {onClick}>{ letter }</button>
        { children }
    </div>;


LetterView.propTypes = {
    children: PropTypes.node,
    letter  : PropTypes.string.isRequired,
    onClick : PropTypes.number.isRequired,
};

export default LetterView;
