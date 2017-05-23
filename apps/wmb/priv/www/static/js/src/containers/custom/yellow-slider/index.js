import React, { PropTypes } from 'react';
import { Slider } from 'react-toolbox/lib/slider';
import theme from './yellow-slider.css';

const YellowSlider = ({ children, ...other }) =>
    <Slider {...other} theme={theme}>
        {children}
    </Slider>;

YellowSlider.propTypes = {
    children: PropTypes.node,
};

export default YellowSlider;
