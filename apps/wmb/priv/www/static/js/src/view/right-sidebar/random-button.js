import React, { PropTypes } from 'react';
import { Button, Switch, Slider } from 'react-toolbox';

const RandomButtonView = ({
    checked,
    onRandomButtonClick,
    onRandomCheckToggle,
    disabled,
    randomNumber,
}) => {
    function onButtonClick(event) {
        event.preventDefault();
        event.stopPropagation();
        onRandomButtonClick();
    }

    return (
        <section className="random">
            <Button
                icon='add'
                label='5 random tracks'
                raised
                primary
                disabled = {disabled}
                floating
                onClick = {onButtonClick}
            />
            <Switch
                checked  = {checked}
                label    = {'autoload'}
                onChange = {onRandomCheckToggle}
            />
            <p>Pinned and with snaps</p>
            <Slider
                pinned
                snaps
                min={1}
                max={7}
                step={1}
                editable
                value={5}
                onChange={() => null}
            />
        </section>
    );
};


RandomButtonView.propTypes = {
    checked            : PropTypes.bool.isRequired,
    disabled           : PropTypes.bool.isRequired,
    onRandomButtonClick: PropTypes.func.isRequired,
    onRandomCheckToggle: PropTypes.func.isRequired,
};

export default RandomButtonView;
