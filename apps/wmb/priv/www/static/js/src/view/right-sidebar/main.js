import React, { PropTypes } from 'react';
import { List } from 'react-toolbox';


const RightSidebarView = ({ children }) =>
    <aside style={{ marginTop: '100px' }}>
        <List>
            { children }
        </List>
    </aside>;


RightSidebarView.propTypes = {
    children: PropTypes.node,
};

export default RightSidebarView;
