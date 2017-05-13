import React, { PropTypes } from 'react';
import { Col } from 'react-bootstrap';

const PlaylistView = ({ children, button }) =>
    <Col xsHidden sm={4} md={3}>
        { button }
        <h1>{ 'Playlist' }</h1>
        { children }
    </Col>;


PlaylistView.propTypes = {
    button  : PropTypes.node,
    children: PropTypes.node,
};

export default PlaylistView;
