import React, { PropTypes } from 'react';
import { Col } from 'react-bootstrap';


const PlaylistView = ({ children }) =>
    <Col xsHidden sm={4} md={3}>
        <h1>{ 'Playlist' }</h1>
        { children }
    </Col>;


PlaylistView.propTypes = {
    children: PropTypes.node,
};

export default PlaylistView;
