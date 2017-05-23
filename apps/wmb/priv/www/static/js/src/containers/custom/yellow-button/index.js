import React, { PropTypes } from 'react';
import { Button } from 'react-toolbox/lib/button';
import theme from './yellow-button.css';

const YellowButton = ({ children, ...other }) =>
    <Button {...other} theme={theme}>
        {children}
    </Button>;

YellowButton.propTypes = {
    children: PropTypes.node,
};

export default YellowButton;
