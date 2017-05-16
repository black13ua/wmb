import React, { PropTypes } from 'react';


const MainView = ({ children }) =>
    <article>
        { children }
    </article>;


MainView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default MainView;
