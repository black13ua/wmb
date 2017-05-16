import React, { PropTypes } from 'react';
import { List, ProgressBar, ListSubHeader, ListItem, Button, Switch, Slider } from 'react-toolbox';

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
            <ListSubHeader caption='Random' />
            <div style = {{ width: '100%', textAlign: 'center', margin: '15px 0' }} >
                <Button
                    icon='add'
                    label={`${randomNumber} random tracks`}
                    raised
                    primary
                    disabled = {disabled}
                    floating
                    onClick = {onButtonClick}
                />
            </div>
            <div style = {{ width: '100%', textAlign: 'center' }} >
                <Switch
                    checked  = {checked}
                    label    = {'autoload'}
                    onChange = {onRandomCheckToggle}
                />
            </div>
            <ListSubHeader caption='Random tracks per request' />
            <Slider
                pinned
                snaps
                min={5}
                max={30}
                step={5}
                editable
                value={randomNumber}
                onChange={onSliderChange.bind(this)}
            />
        </div>
    );
};


RandomButtonView.propTypes = {
    checked            : PropTypes.bool.isRequired,
    disabled           : PropTypes.bool.isRequired,
    onRandomButtonClick: PropTypes.func.isRequired,
    onRandomCheckToggle: PropTypes.func.isRequired,
};

export default RandomButtonView;
