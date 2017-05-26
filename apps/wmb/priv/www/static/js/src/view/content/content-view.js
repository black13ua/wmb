import React, { PropTypes } from 'react';

const contentStyles = {
    display       : 'flex',
    flexWrap      : 'wrap',
    justifyContent: 'space-around',
    alignContent  : 'space-around',
    paddingBottom : '45px',
};


const MainView = ({ children }) =>
    <article style = {contentStyles}>
        { children }
    </article>;


MainView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default MainView;
