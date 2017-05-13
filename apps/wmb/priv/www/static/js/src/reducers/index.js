import { combineReducers } from 'redux';

import musicReduces from './music';
import filtersReduces from './filters';


const rootReducer = combineReducers({
    music  : musicReduces,
    filters: filtersReduces,
});

export default rootReducer;
