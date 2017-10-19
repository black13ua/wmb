import actionCreator from '../utils/actionCreatorFactory';
import * as ActionTypes from '../constants/action-types';


// ******************************************************************************/
// ******************************* FILTERS **************************************/
// ******************************************************************************/
export const fetchFilter                = alias                        => actionCreator(ActionTypes.FETCH_FILTER_DATA, { alias });
export const receiveFilters             = data                         => actionCreator(ActionTypes.RECEIVE_FILTERS_DATA, { data });
export const receiveFilter              = (alias, data)                => actionCreator(ActionTypes.RECEIVE_FILTER_DATA, { alias, data });

export const fetchRandomTracks          = ()                           => actionCreator(ActionTypes.FETCH_RANDOM_TRACKS);

export const fetchSearchResults         = value                        => actionCreator(ActionTypes.FETCH_SEARCH_RESULTS, { value });
export const receiveSearchResults       = tracks                       => actionCreator(ActionTypes.RECEIVE_SEARCH_RESULTS, { tracks });
export const fetching                   = (value, isFetching)          => actionCreator(ActionTypes.FETCHING, { value, isFetching });

export const receiveError               = error                        => actionCreator(ActionTypes.RECEIVE_ERROR, { error: `${error}` });

export const randomCheckToggle          = ()                           => actionCreator(ActionTypes.RANDOM_CHECKER_TOGGLE);
export const saveSearchValue            = value                        => actionCreator(ActionTypes.SAVE_SEARCH_VALUE, { value });
export const randomNumberChange         = value                        => actionCreator(ActionTypes.RANDOM_NUMBER_CHANGE, { value });
export const fetchFiltersData           = ()                           => actionCreator(ActionTypes.FETCH_ALL_FILTERS);


export const fetchAlbumsByFilters       = ()                           => actionCreator(ActionTypes.FETCH_ALBUMS_BY_FILTERS); // eslint-disable-line
export const setFieldValue              = (alias, value)               => actionCreator(ActionTypes.SET_FIELD_VALUE, { alias, value });
export const setSearchValue             = value                        => actionCreator(ActionTypes.SET_SEARCH_VALUE, { value });
export const fetchArtistsByLetter       = letterId                     => actionCreator(ActionTypes.FETCH_ARTISTS_BY_LETTER, { letterId });
export const receiveArtistsByLetter     = (letterId, data)             => actionCreator(ActionTypes.RECEIVE_ARTISTS_BY_LETTER, { letterId, data });
export const fetchAlbumsByArtist        = artistId                     => actionCreator(ActionTypes.FETCH_ALBUMS_BY_ARTIST, { artistId });
export const receiveAlbumsByArtist      = albums                       => actionCreator(ActionTypes.RECEIVE_ALBUMS_BY_ARTIST, { albums });

export const setActiveArtistInAbcFilter = artistId                     => actionCreator(ActionTypes.SET_ACTIVE_ARTIST_IN_ABC_FILTER, { artistId });


// ******************************************************************************/
// ******************************* ALBUMS ***************************************/
// ******************************************************************************/

export const receiveRandomTracks        = tracks                       => actionCreator(ActionTypes.RECEIVE_RANDOM_TRACKS, { tracks });
export const fetchAlbumsByPage          = currentPage                  => actionCreator(ActionTypes.FETCH_ALBUMS_BY_PAGE, { currentPage });
export const receiveAlbums              = albums                       => actionCreator(ActionTypes.RECEIVE_ALBUMS, { albums });

// ******************************************************************************/
// ******************************** VIEW ****************************************/
// ******************************************************************************/
export const selectAlbum                = (albumId, selected)          => actionCreator(ActionTypes.SELECT_ALBUM, { albumId, selected });
export const selectTrack                = (albumId, trackId, selected) => actionCreator(ActionTypes.SELECT_TRACK, { trackId, albumId, selected });
export const clearPlaylist              = ()                           => actionCreator(ActionTypes.CLEAR_PLAYLIST);
export const repeatPlaylist             = ()                           => actionCreator(ActionTypes.REPEAT_PLAYLIST);
export const shufflePlaylist            = ()                           => actionCreator(ActionTypes.SHUFFLE_PLAYLIST);
export const clearWarningState          = ()                           => actionCreator(ActionTypes.CLEAR_WARNING);
export const setWarningState            = warning                      => actionCreator(ActionTypes.SET_WARNING, { warning });
export const setCurrentPage             = currentPage                  => actionCreator(ActionTypes.SET_CURRENT_PAGE, { currentPage });
export const setActiveTrack             = trackId                      => actionCreator(ActionTypes.SET_ACTIVE_TRACK, { trackId });

// ******************************************************************************/
// ******************************* PLAYER ***************************************/
// ******************************************************************************/

export const playTrack                  = track                        => actionCreator(ActionTypes.PLAY_TRACK, { track });
export const stopTrack                  = ()                           => actionCreator(ActionTypes.STOP_TRACK);
export const toggleTrack                = ()                           => actionCreator(ActionTypes.TOGGLE_TRACK);
export const deletePreviousPlayer       = ()                           => actionCreator(ActionTypes.REMOVE_PREVIOUS_PLAYER);

export const onPlayerBuffer             = buffer                       => actionCreator(ActionTypes.ON_PLAYER_BUFFER, { buffer });
export const onPlayerDuration           = duration                     => actionCreator(ActionTypes.ON_PLAYER_DURATION, { duration });
export const onPlayerProgress           = progress                     => actionCreator(ActionTypes.ON_PLAYER_PROGRESS, { progress });
export const onPlayerEnd                = ()                           => actionCreator(ActionTypes.ON_PLAYER_END);
export const prevTrack                  = ()                           => actionCreator(ActionTypes.PREV_TRACK);
export const nextTrack                  = ()                           => actionCreator(ActionTypes.NEXT_TRACK);

export const getPlayerProperty          = (property, value)            => actionCreator(ActionTypes.GET_PLAYER_PROPERTY, { property, value });
export const setPlayerProperty          = (property, value)            => actionCreator(ActionTypes.SET_PLAYER_PROPERTY, { property, value });
export const askPlayerProperty          = property                     => actionCreator(ActionTypes.ASK_PLAYER_PROPERTY, { property });
export const setStoreProperty           = (property, value)            => actionCreator(ActionTypes.SET_STORE_PROPERTY, { property, value });

export const receivePlayerError = error            => actionCreator(ActionTypes.RECEIVE_PLAYER_ERROR, { error: `${error}` });
