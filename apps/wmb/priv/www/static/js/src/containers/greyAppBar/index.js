import React, { PropTypes } from 'react';
import { AppBar } from 'react-toolbox/lib/app_bar';
import theme from './greyAppBar.css';

const PurpleAppBar = ({ children, ...other }) =>
    <AppBar {...other} theme={theme}>
        {children}
    </AppBar>;

PurpleAppBar.propTypes = {
    children: PropTypes.node,
};

export default PurpleAppBar;
