import humps from 'humps';
import {
    ALBUMS_URL,
    RANDOM_URL,
    TRACKS_URL,
    ALBUMS_BY_PAGE,
} from '../constants/urls';


function checkStatus(response) {
    if (response.status >= 200 && response.status < 300) {
        return response;
    }
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
}

export function fetchRandom(randomNumber) {
    return fetch(`${RANDOM_URL}${randomNumber}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchAlbumById(albumId) {
    return fetch(`${ALBUMS_URL}${albumId}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchAlbumsByPage(page) {
    return fetch(`${ALBUMS_BY_PAGE}${page}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}

export function fetchTrack(trackId) {
    return fetch(`${TRACKS_URL}${trackId}`)
        .then(checkStatus)
        .then(res => res.json())
        .then(json => humps.camelizeKeys(json));
}
