import React, { PropTypes } from 'react';
import { Button, FormGroup, ControlLabel, FormControl, HelpBlock } from 'react-bootstrap';


const SearchView = ({ search, onSearchClick, onSearchChange }) => {
    function getValidationState() {
        const length = search.length;
        if (length > 2) return 'success';
        return 'warning';
    }

    return (
        <article className="name">
            <h3 className="filters-header">{ 'Search:' }</h3>

            <form className="filter-name">
                <FormGroup
                    controlId="formBasicText"
                    validationState={getValidationState()}
                >
                    <ControlLabel>{ 'Type your favorit artist' }</ControlLabel>
                    <FormControl
                        placeholder = "Type your favorit artist"
                        type        = "text"
                        value       = {search}
                        onChange    = {onSearchChange}
                    />
                    <FormControl.Feedback />
                    <HelpBlock>3 characters minimum</HelpBlock>
                    <Button
                        bsStyle  = "info"
                        disabled = {search.length < 3}
                        onClick  = {onSearchClick}
                    >
                        { 'Submit' }
                    </Button>
                </FormGroup>
            </form>
        </article>
    );
};


SearchView.propTypes = {
    search        : PropTypes.string,
    onSearchChange: PropTypes.func.isRequired,
    onSearchClick : PropTypes.func.isRequired,
};

export default SearchView;
