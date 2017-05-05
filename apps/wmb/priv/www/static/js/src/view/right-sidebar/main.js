import React, { PropTypes } from 'react';
import { Col } from 'react-bootstrap';


const RightSidebarView = ({ children }) =>
    <Col xsHidden smHidden md={3}>
        { children }
    </Col>;


RightSidebarView.propTypes = {
    children: PropTypes.node,
};

export default RightSidebarView;
