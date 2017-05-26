import React, { PropTypes } from 'react';
import { ListSubHeader, Button, Switch, Slider } from 'react-toolbox';


const RandomButtonView = ({
    checked,
    onRandomButtonClick,
    onRandomCheckToggle,
    disabled,
    randomNumber,
    onSliderChange,
}) => {
    function onButtonClick(event) {
        event.preventDefault();
        event.stopPropagation();
        onRandomButtonClick();
    }

    return (
        <div style = {{ position: 'relative' }} >
            <ListSubHeader caption="Random" />
            <div style = {{ width: '100%', textAlign: 'center', margin: '15px 0' }} >
                <Button
                    floating
                    primary
                    raised
                    disabled = {disabled}
                    icon     = "add"
                    label    = {`${randomNumber} random tracks`}
                    onClick  = {onButtonClick}
                />
            </div>
            <div style = {{ width: '100%', textAlign: 'center' }} >
                <Switch
                    checked  = {checked}
                    label    = {'autoload'}
                    onChange = {onRandomCheckToggle}
                />
            </div>
            <ListSubHeader caption="Random tracks per request" />
            <Slider
                editable
                pinned
                snaps
                max   = {30}
                min   = {5}
                step  = {5}
                value = {randomNumber}
                onChange={onSliderChange.bind(this)}
            />
        </div>
    );
};


RandomButtonView.propTypes = {
    checked            : PropTypes.bool.isRequired,
    disabled           : PropTypes.bool.isRequired,
    randomNumber       : PropTypes.number.isRequired,
    onRandomButtonClick: PropTypes.func.isRequired,
    onRandomCheckToggle: PropTypes.func.isRequired,
    onSliderChange     : PropTypes.func.isRequired,
};

export default RandomButtonView;
