import React, { PropTypes } from 'react';
import classnames from 'classnames';
import { Card, CardTitle, Button, CardMedia, Tooltip, CardActions, List, ListItem } from 'react-toolbox';

const AlbumView = ({ onClick, children, album, artist, cover, date, genre, selected, activeClass }) => {
    const classNames = classnames('righted', { active: activeClass });

    /* const tooltip = (
        <Tooltip id="tooltip">
            <strong>{ `${artist} - ${album} | `}</strong>
            <span>{ `${date} | ${genre}` }</span>
        </Tooltip>
    );*/

    return (
        <Card style={{ width: '350px' }}>
            <CardTitle
                avatar   = {encodeURI(cover)}
                title    = {genre}
                subtitle = {date}
            />
            <CardMedia
                aspectRatio = "wide"
                image       = {encodeURI(cover)}
            />
            <CardTitle
                title    = {album}
                subtitle = {artist}
            />
            <List>
                <ListItem  title="sdfsfsf" />
                <ListItem  title="идф" />
            </List>
            <CardActions>
                <Button label="Add all" />
            </CardActions>
        </Card>
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
