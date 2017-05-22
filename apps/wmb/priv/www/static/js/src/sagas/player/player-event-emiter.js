import { eventChannel } from 'redux-saga';

import {
    receiveError,
    onPlayerBuffer,
    onPlayerDuration,
    onPlayerProgress,
    onPlayerEnd,
} from '../../actions';


export default function createPlayerChannel(player) {
    return eventChannel((emit) => {
        player.on('buffer', buffer     => emit(onPlayerBuffer(buffer)));
        player.on('duration', duration => emit(onPlayerDuration(duration)));
        player.on('progress', progress => emit(onPlayerProgress(progress)));
        player.on('error', error       => emit(receiveError(error)));
        player.on('end', ()            => emit(onPlayerEnd()));
        return () => {};
    });
}
