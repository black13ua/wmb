import React, { PropTypes } from 'react';
import { Col } from 'react-bootstrap';


const MainView = ({ children }) =>
    <Col xs={12} sm={8} md={6}>
        <h1 className="MainView">Albums list</h1>
        { children }
    </Col>;


MainView.propTypes = {
    children: PropTypes.node.isRequired,
};

export default MainView;
