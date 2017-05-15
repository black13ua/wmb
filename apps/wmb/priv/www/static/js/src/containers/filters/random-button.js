import React, { PropTypes } from 'react';
import { connect } from 'react-redux';

import RandomButtonView from '../../view/filters/random-button';

import { fetchRandomTracks, randomCheckToggle } from '../../actions';
import { getIsRandomChecked, getFetchingState } from '../../selectors';


const RandomButtonContainer = ({
    checked,
    handleRandomButtonClick,
    handleRandomCheckToggle,
    fetching,
}) => {
    console.info('fetching', fetching);
    return (
        <RandomButtonView
            checked             = {checked}
            disabled            = {fetching.random}
            onRandomButtonClick = {handleRandomButtonClick}
            onRandomCheckToggle = {handleRandomCheckToggle}
        />
    );
};


RandomButtonContainer.propTypes = {
    checked                : PropTypes.bool.isRequired,
    fetching               : PropTypes.object.isRequired,
    handleRandomButtonClick: PropTypes.func.isRequired,
    handleRandomCheckToggle: PropTypes.func.isRequired,
};

const mapStateToProps = state => ({
    checked : getIsRandomChecked(state),
    fetching: getFetchingState(state),
});

const mapDispatchToProps = dispatch => ({
    handleRandomButtonClick: () => dispatch(fetchRandomTracks()),
    handleRandomCheckToggle: () => dispatch(randomCheckToggle()),
});


export default connect(mapStateToProps, mapDispatchToProps)(RandomButtonContainer);
