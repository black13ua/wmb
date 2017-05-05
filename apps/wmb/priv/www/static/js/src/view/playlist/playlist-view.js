import React from 'react';
import { Col, Table, Button } from 'react-bootstrap';


const PlaylistView = () =>
    <Col xsHidden sm={4} md={3}>
        <Table striped condensed hover>
            <thead>
            <tr>
                <th>#</th>
                <th>First Name</th>
                <th></th>
            </tr>
            </thead>
            <tbody>
            <tr>
                <td>1</td>
                <td>Mark</td>
                <td>
                    <Button bsSize="small" bsStyle="danger">{ 'delete' }</Button>
                </td>
            </tr>
            <tr>
                <td>2</td>
                <td>Jacob</td>
                <td>
                    <Button bsSize="small" bsStyle="danger">{ 'delete' }</Button>
                </td>
            </tr>
            <tr>
                <td>3</td>
                <td>Larry the Bird</td>
                <td>
                    <Button bsSize="small" bsStyle="danger">{ 'delete' }</Button>
                </td>
            </tr>
            </tbody>
        </Table>
    </Col>;

export default PlaylistView;
