import React, { PropTypes } from 'react';

const RightSidebarView = ({ children }) =>
    <aside className="aside-left">
        { children }
    </aside>;


RightSidebarView.propTypes = {
    children: PropTypes.node,
};

export default RightSidebarView;
