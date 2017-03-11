import { combineReducers } from 'redux';
// import { routeReducer } from 'react-router-redux';
import basketReducer from '../common/basket';
import startingServiceMenuReducer from '../common/service-menu';
import liveReducer from './live';
import urlHashReducer from '../common/hasher';
import socketReducer from '../common/sockets';
import jackpotReducer from '../common/jackpot';

const rootReducer = combineReducers({
  basketReducer,
  startingServiceMenuReducer,
  live: liveReducer,
  urlHashReducer,
  sockets: socketReducer,
  jackpot: jackpotReducer,
});

export default rootReducer;
