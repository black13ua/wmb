import { combineReducers } from 'redux';

import musicReduces from './music';
import filtersReduces from './filters';
import playerReduces from './player';


const rootReducer = combineReducers({
    music  : musicReduces,
    filters: filtersReduces,
    player : playerReduces,
});

export default rootReducer;
