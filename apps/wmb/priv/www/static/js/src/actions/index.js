import actionCreator from '../utils/actionCreatorFactory';
import * as ActionTypes from '../constants/action-types';


// ******************************************************************************/
// ******************************* FILTERS **************************************/
// ******************************************************************************/
export const fetchFilter                = alias            => actionCreator(ActionTypes.FETCH_FILTER_DATA, { alias });
export const receiveFilters             = data             => actionCreator(ActionTypes.RECEIVE_FILTERS_DATA, { data });
export const receiveFilter              = (alias, data)    => actionCreator(ActionTypes.RECEIVE_FILTER_DATA, { alias, data });

export const fetchRandomTracks          = ()               => actionCreator(ActionTypes.FETCH_RANDOM_TRACKS);

export const fetchSearchResults         = value            => actionCreator(ActionTypes.FETCH_SEARCH_RESULTS, { value });
export const receiveSearchResults       = tracks           => actionCreator(ActionTypes.RECEIVE_SEARCH_RESULTS, { tracks });
export const fetching                   = value            => actionCreator(ActionTypes.FETCHING, { value });

export const receiveError               = error            => actionCreator(ActionTypes.RECEIVE_ERROR, { error });

export const randomCheckToggle          = ()               => actionCreator(ActionTypes.RANDOM_CHECKER_TOGGLE);
export const saveSearchValue            = value            => actionCreator(ActionTypes.SAVE_SEARCH_VALUE, { value });
export const randomNumberChange         = value            => actionCreator(ActionTypes.RANDOM_NUMBER_CHANGE, { value });
export const fetchFiltersData           = ()               => actionCreator(ActionTypes.FETCH_ALL_FILTERS);


export const setFieldValueIO            = (alias, value)   => actionCreator(ActionTypes.FETCH_ALBUMS_BY_FILTERS, { alias, value }); // eslint-disable-line
export const setFieldValue              = (alias, value)   => actionCreator(ActionTypes.SET_FIELD_VALUE, { alias, value });
export const setSearchValue             = value            => actionCreator(ActionTypes.SET_SEARCH_VALUE, { value });
export const fetchArtistsByLetter       = letterId         => actionCreator(ActionTypes.FETCH_ARTISTS_BY_LETTER, { letterId });
export const receiveArtistsByLetter     = (letterId, data) => actionCreator(ActionTypes.RECEIVE_ARTISTS_BY_LETTER, { letterId, data });
export const fetchAlbumsByArtist        = artistId         => actionCreator(ActionTypes.FETCH_ALBUMS_BY_ARTIST, { artistId });
export const receiveAlbumsByArtist      = albums           => actionCreator(ActionTypes.RECEIVE_ALBUMS_BY_ARTIST, { albums });

export const setActiveArtistInAbcFilter = artistId         => actionCreator(ActionTypes.SET_ACTIVE_ARTIST_IN_ABC_FILTER, { artistId });


// ******************************************************************************/
// ******************************* ALBUMS ***************************************/
// ******************************************************************************/

export const receiveRandomTracks = tracks => actionCreator(ActionTypes.RECEIVE_RANDOM_TRACKS, { tracks });
export const fetchAlbumsByPage   = ()     => actionCreator(ActionTypes.FETCH_ALBUMS_BY_PAGE);
export const receiveAlbums       = albums => actionCreator(ActionTypes.RECEIVE_ALBUMS, { albums });

// ******************************************************************************/
// ******************************** VIEW ****************************************/
// ******************************************************************************/
export const selectAlbum   = (albumId, selected)          => actionCreator(ActionTypes.SELECT_ALBUM, { albumId, selected });
export const selectTrack   = (trackId, albumId, selected) => actionCreator(ActionTypes.SELECT_TRACK, { trackId, albumId, selected });
export const clearPlaylist = ()                           => actionCreator(ActionTypes.CLEAR_PLAYLIST);
