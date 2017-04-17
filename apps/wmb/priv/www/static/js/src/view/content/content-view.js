import React, { PropTypes } from 'react';

const MainView = ({ children }) =>
    <main className="main--center">
        <h1 className="MainView">REACT WILL BE HERE SOON!</h1>
        { children }
    </main>;

MainView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default MainView;
