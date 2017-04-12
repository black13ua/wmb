import React from 'react';
import { PLAYER_HTML } from '../../constants/constants';

const PlayerView = () => {
    function createPlayerInner() {
        return { __html: PLAYER_HTML };
    }

    return (
        <div
            className               = "player--container"
            dangerouslySetInnerHTML = {createPlayerInner()}
        />
    );
};

export default PlayerView;
