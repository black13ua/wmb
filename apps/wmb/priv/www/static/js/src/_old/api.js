import humps from 'humps';
import {
    ALBUMS_URL,
    RANDOM_URL,
    TRACKS_URL,
    FILTER_ABC_URL,
    FILTER_SEARCH_URL,
    RANDOM_NUMBER,
} from './constants';


function fetchRandom() {
    return fetch(`${RANDOM_URL}${RANDOM_NUMBER}`)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(err => console.error(err));
}

function fetchAlbum(albumId) {
    return fetch(`${ALBUMS_URL}${albumId}`)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(err => console.error(err));
}

function fetchTrack(trackId) {
    return fetch(`${TRACKS_URL}${trackId}`)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(err => console.error(err));
}

function fetchSearchResults(searchText) {
    return fetch(`${FILTER_SEARCH_URL}${searchText}`)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(err => console.error(err));
}

function fetchFilterABC() {
    return fetch(`${FILTER_ABC_URL}`)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json))
        .catch(err => console.error(err));
}

const API = {
    fetchRandom,
    fetchAlbum,
    fetchTrack,
    fetchFilterABC,
    fetchSearchResults,
};

export default API;