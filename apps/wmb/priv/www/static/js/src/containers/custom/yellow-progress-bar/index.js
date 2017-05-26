import React, { PropTypes } from 'react';
import { ProgressBar } from 'react-toolbox/lib/progress_bar';
import theme from './yellow-progress-bar.css';

const YellowButton = ({ children, ...other }) =>
    <ProgressBar {...other} theme={theme}>
        {children}
    </ProgressBar>;

YellowButton.propTypes = {
    children: PropTypes.node,
};

export default YellowButton;
