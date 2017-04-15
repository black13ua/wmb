import actionCreator from '../utils/actionCreatorFactory';
import * as ActionTypes from '../constants/action-types';

export const fetchABCfilter       = ()     => actionCreator(ActionTypes.FETCH_ABC_FILTER);
export const receiveABCfilter     = abc    => actionCreator(ActionTypes.RECEIVE_ABC_FILTER, { abc });
export const fetchGenresFilter    = ()     => actionCreator(ActionTypes.FETCH_GENRES_FILTER);
export const receiveGenresFilter  = genres => actionCreator(ActionTypes.RECEIVE_GENRES_FILTER, { genres });
export const fetchDatesFilter     = ()     => actionCreator(ActionTypes.FETCH_DATES_FILTER);
export const receiveDatesFilter   = dates  => actionCreator(ActionTypes.RECEIVE_DATES_FILTER, { dates });
export const fetchRandomTracks    = ()     => actionCreator(ActionTypes.FETCH_RANDOM_TRACKS);
export const receiveRandomTracks  = tracks => actionCreator(ActionTypes.RECEIVE_RANDOM_TRACKS, { tracks });
export const randomCheckToggle    = ()     => actionCreator(ActionTypes.RANDOM_CHECKER_TOGGLE);
export const fetchSearchResults   = value  => actionCreator(ActionTypes.FETCH_SEARCH_RESULTS, { value });
export const receiveSearchResults = tracks => actionCreator(ActionTypes.RECEIVE_SEARCH_RESULTS, { tracks });
export const saveSearchValue      = value  => actionCreator(ActionTypes.SAVE_SEARCH_VALUE, { value });
export const fetching             = value  => actionCreator(ActionTypes.FETCHING, { value });
