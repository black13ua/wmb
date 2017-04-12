import React, { Component } from 'react';
import { connect } from 'react-redux';

import MainView from '../../components/main/main-view';


class MainContainer extends Component {
    render() {
        return (
            <MainView />
        );
    }
}

MainContainer.propTypes = {

};

const mapStateToProps = () => ({
});

const mapDispatchToProps = () => ({
});


export default connect(mapStateToProps, mapDispatchToProps())(MainContainer);
