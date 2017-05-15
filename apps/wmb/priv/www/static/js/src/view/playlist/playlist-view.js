import React, { PropTypes } from 'react';
import { List, ListItem, Chip, Avatar, IconButton } from 'react-toolbox';

const PlaylistView = ({ children, button }) =>
    <aside style={{ marginTop: '100px' }}>
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
