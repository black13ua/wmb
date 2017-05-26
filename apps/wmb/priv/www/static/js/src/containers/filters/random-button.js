import React, { PropTypes } from 'react';
import { connect } from 'react-redux';

import RandomButtonView from '../../view/filters/random-button';

import { fetchRandomTracks, randomCheckToggle, randomNumberChange } from '../../actions';
import { getIsRandomChecked, getFetchingState, getRandomNumber } from '../../selectors';


const RandomButtonContainer = ({
    checked,
    handleRandomButtonClick,
    handleRandomCheckToggle,
    fetching,
    handleSliderChange,
    randomNumber,
}) =>
    <RandomButtonView
        checked             = {checked}
        disabled            = {fetching.random}
        onRandomButtonClick = {handleRandomButtonClick}
        onRandomCheckToggle = {handleRandomCheckToggle}
        onSliderChange      = {handleSliderChange}
        randomNumber        = {randomNumber}
    />;


RandomButtonContainer.propTypes = {
    checked                : PropTypes.bool.isRequired,
    fetching               : PropTypes.object.isRequired,
    handleRandomButtonClick: PropTypes.func.isRequired,
    handleRandomCheckToggle: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    checked     : getIsRandomChecked(state),
    fetching    : getFetchingState(state),
    randomNumber: getRandomNumber(state),
});

const mapDispatchToProps = dispatch => ({
    handleRandomButtonClick: () => dispatch(fetchRandomTracks()),
    handleRandomCheckToggle: () => dispatch(randomCheckToggle()),
    handleSliderChange     : value => dispatch(randomNumberChange(value)),
});


export default connect(mapStateToProps, mapDispatchToProps)(RandomButtonContainer);
