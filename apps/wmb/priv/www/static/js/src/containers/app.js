import React, { Component } from 'react';
// import { connect }  from 'react-redux';
// import { createStructuredSelector } from 'reselect';

// import { getAlbumsIds } from '../../selectors';


class App extends Component { // eslint-disable-line
    render() {
        return (
            <div className = "wrapper--container">
                <h1>{ 'Hello from React!' }</h1>
            </div>
        );
    }
}


// App.propTypes = {
//     something: PropTypes.any.isRequired,
// };

// const mapStateToProps = createStructuredSelector({
//     albums: getAlbumsIds,
// });

// export default connect(mapStateToProps)(App);
export default App;
