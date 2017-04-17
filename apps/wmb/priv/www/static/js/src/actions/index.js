import actionCreator from '../utils/actionCreatorFactory';
import * as ActionTypes from '../constants/action-types';


// ******************************************************************************/
// ******************************* FILTERS **************************************/
// ******************************************************************************/
export const fetchFilter          = alias         => actionCreator(ActionTypes.FETCH_FILTER_DATA, { alias });
export const receiveFilter        = (alias, data) => actionCreator(ActionTypes.RECEIVE_FILTER_DATA, { alias, data });

export const fetchRandomTracks    = ()            => actionCreator(ActionTypes.FETCH_RANDOM_TRACKS);

export const fetchSearchResults   = value         => actionCreator(ActionTypes.FETCH_SEARCH_RESULTS, { value });
export const receiveSearchResults = tracks        => actionCreator(ActionTypes.RECEIVE_SEARCH_RESULTS, { tracks });
export const fetching             = value         => actionCreator(ActionTypes.FETCHING, { value });

export const randomCheckToggle = ()             => actionCreator(ActionTypes.RANDOM_CHECKER_TOGGLE);
export const saveSearchValue   = value          => actionCreator(ActionTypes.SAVE_SEARCH_VALUE, { value });
export const setFieldValueIO   = (alias, value) => actionCreator(ActionTypes.FETCH_ALBUMS_BY_FILTERS, { alias, value });
export const setFieldValue     = (alias, value) => actionCreator(ActionTypes.SET_FIELD_VALUE, { alias, value });
export const setSearchValue    = value          => actionCreator(ActionTypes.SET_SEARCH_VALUE, { value });


// ******************************************************************************/
// ******************************* ALBUMS ***************************************/
// ******************************************************************************/

export const receiveRandomTracks = tracks => actionCreator(ActionTypes.RECEIVE_RANDOM_TRACKS, { tracks });
export const fetchAlbumsByPage   = ()     => actionCreator(ActionTypes.FETCH_ALBUMS_BY_PAGE);
export const receiveAlbums       = albums => actionCreator(ActionTypes.RECEIVE_ALBUMS, { albums });

