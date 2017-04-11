import { combineReducers } from 'redux';

import songsReduces from './songs';


const rootReducer = combineReducers({
    songs: songsReduces,
});

export default rootReducer;
