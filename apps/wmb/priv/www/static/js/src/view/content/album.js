import React, { PropTypes } from 'react';
import { customEncode } from '../../utils/custom-encode';

import { Card, CardTitle, CardMedia, Button, CardActions, List, ListItem } from 'react-toolbox';
import YellowButton from '../../containers/custom/yellow-button';


const AlbumView = ({ handleUnfold, folded, onClick, children, album, artist, cover, date, genre, selected }) => {
    return (
        <section style={{ flex: '0 350px', padding: '1em 0' }}>
            <Card style={{ width: '350px', background: 'lightgrey' }}>
                <CardTitle
                    title    = {artist}
                    subtitle = {`${date} - ${album}`}
                />
                <CardMedia
                    aspectRatio = {'square'}
                    style = {{ width: '300px', margin: 'auto' }}
                >
                    <img src={customEncode(cover)} alt={album} />
                </CardMedia>
                <div style={{ position: 'relative', padding: '0 20px' }} >
                    <CardTitle
                        style={{ position: 'absolute', top: 0, right: '20px' }}
                        title    = {genre}
                    />
                    <CardActions>
                        {!selected
                            ? (<Button
                                raised
                                primary
                                label="Add album"
                                onClick = {onClick}
                            />)
                            : (<YellowButton raised accent label="Remove album" onClick = {onClick} />)
                        }
                    </CardActions>
                </div>
                <List selectable ripple>
                    <ListItem
                        caption='Tracks'
                        onClick = {handleUnfold}
                        rightIcon={!folded ? 'keyboard_arrow_down' : 'keyboard_arrow_right'}
                        leftIcon={'assignment'}
                    />
                    { children }
                </List>
            </Card>
        </section>
    );
};

AlbumView.propTypes = {
    activeClass: PropTypes.bool,
    album      : PropTypes.string.isRequired,
    artist     : PropTypes.string.isRequired,
    children   : PropTypes.node,
    cover      : PropTypes.string.isRequired,
    date       : PropTypes.string.isRequired,
    genre      : PropTypes.string.isRequired,
    selected   : PropTypes.bool.isRequired,
    onClick    : PropTypes.func.isRequired,
};

export default AlbumView;
