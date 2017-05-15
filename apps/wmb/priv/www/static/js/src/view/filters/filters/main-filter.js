import React, { PropTypes } from 'react';
import { List, ListSubHeader, ListDivider } from 'react-toolbox';


const FiltersView = ({ children }) =>
    <div>
        <List ripple>
            <ListDivider />
            <ListSubHeader caption='Filters' />
            { children }
        </List>
    </div>;

FiltersView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default FiltersView;
