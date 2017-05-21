import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';

import PlaylistTrackView from '../../view/playlist/track';

import { selectTrack } from '../../actions';
import { makeSelectTrackDatabyId } from '../../selectors';


// import debugRender from 'react-render-debugger';
// @debugRender
class PlaylistTrackContainer extends Component {
    handleSelectTrackClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.selectTrack();
    }

    render() {
        const { trackData } = this.props;
        if (_.isEmpty(trackData)) return null;

        return (
            <PlaylistTrackView
                {...trackData}
                onClick  = {this.handleSelectTrackClick}
            />
        );
    }
}


PlaylistTrackContainer.propTypes = {
    selectTrack: PropTypes.func.isRequired,
    trackData  : PropTypes.object,
};

const makeMapStateToProps = () => {
    const selectTrackDatabyId = makeSelectTrackDatabyId();

    return createStructuredSelector({
        trackData: selectTrackDatabyId,
    });
};

const mapDispatchToProps = (dispatch, ownProps) => ({
    selectTrack: () => dispatch(selectTrack(ownProps.trackId, ownProps.album, true)),
});

export default connect(makeMapStateToProps, mapDispatchToProps)(PlaylistTrackContainer);
