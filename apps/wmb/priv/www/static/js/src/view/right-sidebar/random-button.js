import React, { PropTypes } from 'react';
import { Button, FormGroup } from 'react-bootstrap';

import Switch from 'react-bootstrap-switch';

const RandomButtonView = ({
    checked,
    onRandomButtonClick,
    onRandomCheckToggle,
    disabled,
}) => {
    function onButtonClick(event) {
        event.preventDefault();
        event.stopPropagation();
        onRandomButtonClick();
    }

    return (
        <section className="random">
            <FormGroup>
                <Button
                    bsStyle  = "info"
                    disabled = {disabled}
                    onClick  = {onButtonClick}
                >
                    { '+5 Random Tracks' }
                </Button>
            </FormGroup>
            <FormGroup>
                <Switch
                    animate
                    defaultValue
                    labelText = {'autoload'}
                    offColor  = {'warning'}
                    value     = {checked}
                    onChange  = {onRandomCheckToggle}
                    onColor   = {'success'}
                />
            </FormGroup>
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
