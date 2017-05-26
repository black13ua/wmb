import humps from 'humps';
import {
    FILTER_ABC_URL,
    FILTER_GENRES_URL,
    FILTER_DATES_URL,
    FILTER_SEARCH_URL,
    DATA_BY_FILTERS_URL,
    DATA_BY_SEARCH_URL,
    ARTISTS_BY_LETTER_URL,
    ALBUMS_BY_ARTIST_URL,
} from '../constants/urls';


function checkStatus(response) {
    if (response.status >= 200 && response.status < 300) {
        return response;
    }
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
}


export function fetchSearchResults(searchText) {
    return fetch(`${FILTER_SEARCH_URL}${searchText}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchAbcFilter() {
    return fetch(`${FILTER_ABC_URL}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchGenresFilter() {
    return fetch(`${FILTER_GENRES_URL}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchDatesFilter() {
    return fetch(`${FILTER_DATES_URL}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchDataByFilters(filters) {
    return fetch(`${DATA_BY_FILTERS_URL}`, {
        method: 'post',
        body  : JSON.stringify(filters),
    })
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchDataBySearch(search) {
    return fetch(`${DATA_BY_SEARCH_URL}`, {
        method: 'post',
        body  : JSON.stringify({ search }),
    })
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchArtistsByLetter(letterId) {
    return fetch(`${ARTISTS_BY_LETTER_URL}${letterId}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchAlbumsByArtist(artistId) {
    return fetch(`${ALBUMS_BY_ARTIST_URL}${artistId}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}
