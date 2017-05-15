import React, { PropTypes } from 'react';
import { List, ListSubHeader, ListItem, Button, Switch, Slider } from 'react-toolbox';

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
        <List ripple>
            <ListSubHeader caption='Random' />
            <div style = {{ width: '100%', textAlign: 'center', margin: '15px 0' }} >
                <Button
                    icon='add'
                    label='5 random tracks'
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
                min={1}
                max={7}
                step={1}
                editable
                value={5}
                onChange={() => null}
            />
        </List>
    );
};


RandomButtonView.propTypes = {
    checked            : PropTypes.bool.isRequired,
    disabled           : PropTypes.bool.isRequired,
    onRandomButtonClick: PropTypes.func.isRequired,
    onRandomCheckToggle: PropTypes.func.isRequired,
};

export default RandomButtonView;
