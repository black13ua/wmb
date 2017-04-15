import React, { PropTypes } from 'react';

const RandomButtonView = ({ checked, onRandomButtonClick, onRandomCheckToggle }) =>
    <section className="random">
        <button
            className = "add-random"
            onClick   = {onRandomButtonClick}
        >
            { '+5 Random Tracks' }
        </button>
        <label htmlFor="autoload">
            <span>{ 'autoload' }</span>
            <input
                checked  = {checked}
                type     = "checkbox"
                onChange = {onRandomCheckToggle}
            />
        </label>
    </section>;


RandomButtonView.propTypes = {
    checked            : PropTypes.bool.isRequired,
    onRandomButtonClick: PropTypes.func.isRequired,
    onRandomCheckToggle: PropTypes.func.isRequired,
};

export default RandomButtonView;
