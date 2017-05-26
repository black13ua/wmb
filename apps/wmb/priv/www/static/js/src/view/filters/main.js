import React, { PropTypes } from 'react';
import { List } from 'react-toolbox';


const FIltersMenuView = ({ children }) =>
    <aside style={{ marginTop: '70px' }}>
        <List>
            { children }
        </List>
    </aside>;


FIltersMenuView.propTypes = {
    children: PropTypes.node,
};

export default FIltersMenuView;
