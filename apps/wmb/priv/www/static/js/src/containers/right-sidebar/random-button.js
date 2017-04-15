import React, { PropTypes } from 'react';
import { connect } from 'react-redux';

import RandomButtonView from '../../view/right-sidebar/random-button';

import { fetchRandomTracks, randomCheckToggle } from '../../actions';
import { getIsRandomChecked } from '../../selectors';


const RandomButtonContainer = ({ checked, handleRandomButtonClick, handleRandomCheckToggle }) =>
    <RandomButtonView
        checked             = {checked}
        onRandomButtonClick = {handleRandomButtonClick}
        onRandomCheckToggle = {handleRandomCheckToggle}
    />;


RandomButtonContainer.propTypes = {
    checked                : PropTypes.bool.isRequired,
    handleRandomButtonClick: PropTypes.func.isRequired,
    handleRandomCheckToggle: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    checked: getIsRandomChecked(state),
});

const mapDispatchToProps = dispatch => ({
    handleRandomButtonClick: () => dispatch(fetchRandomTracks()),
    handleRandomCheckToggle: () => dispatch(randomCheckToggle()),
});


export default connect(mapStateToProps, mapDispatchToProps)(RandomButtonContainer);
