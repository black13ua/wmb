import React, { PropTypes } from 'react';


const MainView = ({ children }) =>
    <article style = {{ display: 'flex', flexWrap: 'wrap', justifyContent: 'space-around', alignContent: 'space-around' }}>
        { children }
    </article>;


MainView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default MainView;
