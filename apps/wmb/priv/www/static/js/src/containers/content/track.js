import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';

import TrackView from '../../view/content/track';

import { selectTrack } from '../../actions';
import { makeSelectTrackDatabyId } from '../../selectors';


class TrackContainer extends Component {
    handleSelectTrackClick = (event) => {
        event.preventDefault();
        event.stopPropagation();
        this.props.selectTrack();
    }

    render() {
        const { trackData } = this.props;
        if (_.isEmpty(trackData)) return null;

        return (
            <TrackView
                {...trackData}
                onClick = {this.handleSelectTrackClick}
            />
        );
    }
}


TrackContainer.propTypes = {
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
