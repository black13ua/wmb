import API from './api';
import { TRACK_POINT_TO_EXTRA_LOAD } from './constants';

let playerAPI = {};
let enableRandomAdd = true;

/* eslint-disable */
function createPlayerAPI(playlist = []) {
    var _sampleRate = (function() {
        var AudioContext = (window.AudioContext || window.webkitAudioContext);
        if (!AudioContext) {
            return 44100;
        }
        return new AudioContext().sampleRate;
    }());
    return (function(DGPlayer) {
        if (!DGPlayer) return;
        DGPlayer.playlist = playlist;
        DGPlayer.volume = 50;

        var player, onplay, onplaylist;
 
        DGPlayer.on('play', onplay = function(a,b,c){
            if (player) {
                player.disconnect();
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(DGPlayer.current.url), DGPlayer);
            DGPlayer.off('play', onplay);
        });

        DGPlayer.on('playlist', onplaylist = function(a,b,c) {
            if(player) {
                player.disconnect();
                DGPlayer.state = 'paused';
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(DGPlayer.current.url), DGPlayer);
        });
        return DGPlayer;
    })(window.DGPlayer(document.getElementById('dgplayer'), callback, TRACK_POINT_TO_EXTRA_LOAD)); // callback for uploading extra tracks
};
/* eslint-enable */

export function createPlayer() {
    playerAPI = createPlayerAPI();
}

export function trackToggle(trackId, active) {
    if (active) {
        API.fetchTrack(trackId)
            .then((json) => {
                addTrackToPlaylist(json);
            });
    } else {
        removeTrackFromPlaylist(trackId);
    }
}

export function albumToggle(albumId, active) {
    if (active) {
        API.fetchAlbum(albumId)
            .then((json) => {
                json.tracks.forEach((track) => {
                    const flattedJson = { ...json, ...track };
                    addTrackToPlaylist(flattedJson);
                });
            });
    } else {
        removeAlbumFromPlaylist(albumId); // TODO: get all tracksId from album and remove them separatly
    }
}

export function randomAdd() {
    API.fetchRandom()
        .then((json) => {
            json.forEach((track) => {
                addTrackToPlaylist(track);
            });
        });
}

function removeAlbumFromPlaylist() {
    // TODO
}

function addTrackToPlaylist(json) {
    const { trackId, artist, title, file, cover, album } = json;
    playerAPI.addSong = {
        _id    : trackId,
        name   : `${artist} - ${title}`,
        url    : encodeURI(file),
        artist,
        album,
        picture: encodeURI(cover)
    };
}

function removeTrackFromPlaylist(id) {
    if (!playerAPI) return;
    const songToRemove = playerAPI.playlist.find(song => song._id === id);
    const indexOfSong = playerAPI.playlist.indexOf(songToRemove);
    if (indexOfSong < 0) {
        return;
    }
    playerAPI.removeSong = indexOfSong;
}

window.autoLoadToggle = () => { // FIX: will be fixed with react implementation
    enableRandomAdd = !enableRandomAdd;
};

function callback(type) {
    switch (type) {
        case 'moreTracks?': {
            if (enableRandomAdd) randomAdd();
            break;
        }
        default: return null;
    }
}
