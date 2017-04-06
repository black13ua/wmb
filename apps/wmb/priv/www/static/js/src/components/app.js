import React, { PropTypes, Component } from 'react';
import { connect }  from 'react-redux';
import { createStructuredSelector } from 'reselect';

import { getAlbumsIds } from '../../selectors';


class App extends Component {
    get albumsList() {
        return null;
    }
    render() {
        <h1>Hello from React!</h1>
        <div>{ this.albumsList }</div>
    }
}


App.propTypes = {
    something: PropTypes.any.isRequired,
}

const mapStateToProps = createStructuredSelector({
    albums: getAlbumsIds,
});

export default connect(mapStateToProps)(App);