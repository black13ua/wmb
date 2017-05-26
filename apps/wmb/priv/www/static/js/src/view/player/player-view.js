import React from 'react';
import { Navbar, Nav, NavItem, NavDropdown, MenuItem } from 'react-bootstrap';

import { PLAYER_HTML } from '../../constants/constants';

const PlayerView = () => {
    function createPlayerInner() {
        return { __html: PLAYER_HTML };
    }

    return (
        <Navbar inverse collapseOnSelect>
            <Navbar.Header>
                <Navbar.Brand>
                    <a href="#">Player</a>
                </Navbar.Brand>
                <Navbar.Toggle />
            </Navbar.Header>
            <Navbar.Collapse>
                <Nav>
                    <NavItem eventKey={1} href="#">Play</NavItem>
                    <NavItem eventKey={2} href="#">Link</NavItem>
                    <NavDropdown eventKey={3} title="Settings" id="basic-nav-dropdown">
                        <MenuItem eventKey={3.1}>Action</MenuItem>
                        <MenuItem eventKey={3.2}>Another action</MenuItem>
                        <MenuItem eventKey={3.3}>Something else here</MenuItem>
                        <MenuItem divider />
                        <MenuItem eventKey={3.3}>Separated link</MenuItem>
                    </NavDropdown>
                </Nav>
                <Nav pullRight>
                    <NavItem eventKey={1} href="#">Next</NavItem>
                    <NavItem eventKey={2} href="#">Prev</NavItem>
                </Nav>
            </Navbar.Collapse>
        </Navbar>
        /* <div
            className               = "player--container"
            dangerouslySetInnerHTML = {createPlayerInner()}
        />*/
    );
};

export default PlayerView;
