import React, { PropTypes } from 'react';
import { Dialog } from 'react-toolbox';


const CleanPlaylistDialog = ({ actions, onCancelClick, active }) =>
    <Dialog
        actions        = {actions}
        active         = {active}
        title          = "Think twice"
        onEscKeyDown   = {onCancelClick}
        onOverlayClick = {onCancelClick}
    >
        <p>{ 'Do you realy want to delete all tracks from playlist?' }</p>
    </Dialog>;


CleanPlaylistDialog.propTypes = {
    actions      : PropTypes.arrayOf(PropTypes.object).isRequired,
    active       : PropTypes.bool.isRequired,
    onCancelClick: PropTypes.func.isRequired,
};

export default CleanPlaylistDialog;
