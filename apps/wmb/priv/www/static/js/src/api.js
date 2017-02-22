import { ALBUMS_URL, RANDOM_URL, TRACKS_URL } from './constants';


export function fetchRandom() {
    return fetch(`${RANDOM_URL}`)
        .then(res => res.json())
        .catch(err => console.error(err));
}

export function fetchAlbum(albumId) {
    return fetch(`${ALBUMS_URL}${albumId}`)
        .then(res => res.json())
        .catch(err => console.error(err));
}

export function fetchTrack(trackId) {
    return fetch(`${TRACKS_URL}${trackId}`)
        .then(res => res.json())
        .catch(err => console.error(err));
}
