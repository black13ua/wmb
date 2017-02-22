import { ALBUMS_URL, RANDOM_URL, TRACKS_URL } from './constants';

function handleWrongStatusCoode(res) {
    if (res.status !== 200) {
        throw new Error(res.url.split(`/`).slice(3).join(`/`) + `>`
            + res.status + `:` + res.statusText
        );
    }
    return res;
}

export function fetchRandom() {
    return fetch(`${ RANDOM_URL }`)
        .then(handleWrongStatusCoode)
        .then((res) => res.json())
        .catch((err) => console.error(err));
}

export function fetchAlbum(albumId) {
    return fetch(`${ ALBUMS_URL }${ albumId }`)
        .then(handleWrongStatusCoode)
        .then((res) => res.json())
        .catch((err) => console.error(err));
}

export function fetchTrack(trackId) {
    return fetch(`${ RANDOM_URL }${ trackId }`)
        .then(handleWrongStatusCoode)
        .then((res) => res.json())
        .catch((err) => console.error(err));
}