import React, { PropTypes } from 'react';


const MainView = ({ children }) =>
    <article>
        <h1 className="MainView">{ 'Albums' }</h1>
        { children }
    </article>;


MainView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default MainView;
