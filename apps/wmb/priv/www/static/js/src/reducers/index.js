import { combineReducers } from 'redux';

import albumsReduces from './albums';
import filtersReduces from './filters';


const rootReducer = combineReducers({
    albums : albumsReduces,
    filters: filtersReduces,
});

export default rootReducer;
