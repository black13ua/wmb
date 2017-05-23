import { createSelector } from 'reselect';

const playerSelector = state => state.player;

export const getPlayerBuffer =  createSelector(
    playerSelector,
    state => state.buffer
);

export const getPlayerDuration =  createSelector(
    playerSelector,
    state => state.duration
);

export const getPlayerProgress =  createSelector(
    playerSelector,
    state => state.progress
);

export const getPlayerIsPlaying =  createSelector(
    playerSelector,
    state => state.playing
);

export const getPlayerVolume =  createSelector(
    playerSelector,
    state => state.volume
);
