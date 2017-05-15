import React, { PropTypes } from 'react';
import { List, ListItem } from 'react-toolbox';

const PlaylistView = ({ children, button }) =>
    <aside style={{ marginTop: '100px', padding: '10px' }}>
        <List selectable ripple>
            <ListItem
                caption='Playlist'
            >
                { button }
            </ListItem>
            { children }
        </List>
    </aside>;


PlaylistView.propTypes = {
    button  : PropTypes.node,
    children: PropTypes.node,
};

export default PlaylistView;
