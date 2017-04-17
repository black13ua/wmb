import humps from 'humps';
import {
    ALBUMS_URL,
    RANDOM_URL,
    TRACKS_URL,
    FILTER_ABC_URL,
    FILTER_GENRES_URL,
    FILTER_DATES_URL,
    FILTER_SEARCH_URL,
    DATA_BY_FILTERS_URL,
    DATA_BY_SEARCH_URL,
    ALBUMS_BY_PAGE,
} from '../constants/urls';
import { RANDOM_NUMBER } from '../constants/constants';


function checkStatus(response) {
    if (response.status >= 200 && response.status < 300) {
        return response;
    }
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
}

function fetchRandom() {
    return fetch(`${RANDOM_URL}${RANDOM_NUMBER}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchAlbum(albumId) {
    return fetch(`${ALBUMS_URL}${albumId}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchAlbumsByPage(page) {
    return fetch(`${ALBUMS_BY_PAGE}${page}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchTrack(trackId) {
    return fetch(`${TRACKS_URL}${trackId}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchSearchResults(searchText) {
    return fetch(`${FILTER_SEARCH_URL}${searchText}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchAbcFilter() {
    return fetch(`${FILTER_ABC_URL}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchGenresFilter() {
    return fetch(`${FILTER_GENRES_URL}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchDatesFilter() {
    return fetch(`${FILTER_DATES_URL}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchDataByFilters(filters) {
    return fetch(`${DATA_BY_FILTERS_URL}`, {
        method: 'post',
        body  : JSON.stringify(filters),
    })
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

function fetchDataBySearch(search) {
    return fetch(`${DATA_BY_SEARCH_URL}`, {
        method: 'post',
        body  : JSON.stringify({ search }),
    })
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(error => console.log(`%c ${error.message}`, 'color: deepPink'));
}

const API = {
    fetchRandom,
    fetchAlbum,
    fetchTrack,
    fetchAbcFilter,
    fetchGenresFilter,
    fetchDatesFilter,
    fetchSearchResults,
    fetchDataByFilters,
    fetchDataBySearch,
    fetchAlbumsByPage,
};

export default API;
