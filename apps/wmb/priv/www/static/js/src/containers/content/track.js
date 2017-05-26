import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';

import TrackView from '../../view/content/track';

import { selectTrack } from '../../actions';
import { makeSelectTrackDatabyId } from '../../selectors';

// import debugRender from 'react-render-debugger';
// @debugRender
class TrackContainer extends Component {
    handleSelectTrackClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.selectTrack();
    }

    render() {
        const { trackData, selected } = this.props;
        if (_.isEmpty(trackData)) return null;

        return (
            <TrackView
                {...trackData}
                selected = {selected}
                onClick  = {this.handleSelectTrackClick}
            />
        );
    }
}


TrackContainer.propTypes = {
    selected   : PropTypes.bool.isRequired,
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
    selectTrack: () => dispatch(selectTrack(ownProps.trackId, ownProps.album, ownProps.selected)),
});

export default connect(makeMapStateToProps, mapDispatchToProps)(TrackContainer);
