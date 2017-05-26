import React, { PropTypes } from 'react';
import { connect } from 'react-redux';
import { createStructuredSelector } from 'reselect';

import YellowProgressBar from '../custom/yellow-progress-bar';
import { getPlayerBuffer, getPlayerProgress } from '../../selectors';


const ProgressBarContainer = ({ buffer, progress }) =>
    <div style = {{ width: '100%', position: 'fixed', left: 0, top: '60px' }} >
        <YellowProgressBar
            multicolor
            buffer = {buffer}
            mode   = "determinate"
            value  = {progress}
        />
    </div>;


ProgressBarContainer.propTypes = {
    buffer  : PropTypes.number.isRequired,
    progress: PropTypes.number.isRequired,
};

const mapStateToProps = createStructuredSelector({
    buffer  : getPlayerBuffer,
    progress: getPlayerProgress,
});

export default connect(mapStateToProps)(ProgressBarContainer);
