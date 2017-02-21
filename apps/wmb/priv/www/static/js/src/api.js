import { ALBUMS_URL, RANDOM_URL, TRACKS_URL } from './constants';

function handleWrongStatusCoode(res) {
    if (res.status !== 200) {
        throw new Error(res.url.split(`/`).slice(3).join(`/`) + `>`
            + res.status + `:` + res.statusText
        );
    }
    return res;
}

function fetchRandomTracks() {
    return fetch(`${ RANDOM_URL }`)
        .then(handleWrongStatusCoode)
        .then((res) => res.json())
        .catch((err) => console.error(err));
}

function fetchFullAlbum(albumId) {
    return fetch(`${ ALBUMS_URL }${ albumId }`)
        .then(handleWrongStatusCoode)
        .then((res) => res.json())
        .catch((err) => console.error(err));
}

function fetchSingleTrack(trackId) {
    return fetch(`${ RANDOM_URL }${ trackId }`)
        .then(handleWrongStatusCoode)
        .then((res) => res.json())
        .catch((err) => console.error(err));
}