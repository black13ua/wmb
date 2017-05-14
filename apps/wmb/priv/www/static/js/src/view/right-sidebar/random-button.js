import React, { PropTypes } from 'react';
import { Button, FormGroup } from 'react-bootstrap';

// import Switch from 'react-bootstrap-switch';
import Switch from 'react-toolbox/lib/switch';

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
                    checked     = {checked}
                    label = {'autoload'}
                    onChange  = {onRandomCheckToggle}
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
