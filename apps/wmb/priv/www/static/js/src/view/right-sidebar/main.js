import React, { PropTypes } from 'react';


const RightSidebarView = ({ children }) =>
    <aside style={{ marginTop: '100px' }}>
        { children }
    </aside>;


RightSidebarView.propTypes = {
    children: PropTypes.node,
};

export default RightSidebarView;
