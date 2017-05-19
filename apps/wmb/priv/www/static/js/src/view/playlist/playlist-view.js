import React, { PropTypes } from 'react';
import { List, ListItem } from 'react-toolbox';

const PlaylistView = ({ children, button }) =>
    <section className="playlist--wrapper" style={{ margin: '70px 0 30px', padding: '10px' }}>
        <List selectable ripple>
            <ListItem
                caption='Playlist'
            >
                { button }
            </ListItem>
            { children }
        </List>
    </section>;


PlaylistView.propTypes = {
    button  : PropTypes.node,
    children: PropTypes.node,
};

export default PlaylistView;
