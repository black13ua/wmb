import React, { PropTypes } from 'react';
import { List, ListItem, Chip, Avatar, IconButton } from 'react-toolbox';

const PlaylistView = ({ children, button }) =>
    <aside style={{ 'margin-top': '100px' }}>
        <List selectable ripple>
            <ListItem
                caption='Playlist'
            >
                { button }
            </ListItem>
            <Chip
                style={{ display: 'block', margin: '3px', cursor: 'pointer' }}
                deletable
                onDeleteClick={() => null}
            >
                <Avatar style={{ backgroundColor: 'deepskyblue' }} icon="play_arrow" />
                <span>Avatar Chip</span>
            </Chip>
            <Chip
                style={{ display: 'block', margin: '3px'  }}
                deletable
                onDeleteClick={() => null}
            >
                <Avatar style={{ backgroundColor: 'deeppink' }} icon="music_note" />
                <span>Avatar Chip</span>
            </Chip>
            { children }
        </List>
    </aside>;


PlaylistView.propTypes = {
    button  : PropTypes.node,
    children: PropTypes.node,
};

export default PlaylistView;
