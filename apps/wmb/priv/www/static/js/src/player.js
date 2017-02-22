let playerAPI = {};

import {
    fetchAlbum,
    fetchTrack,
    fetchRandom
} from './api.js';

export function createPlayer(playlist = []) {
    var _sampleRate = (function() {
        var AudioContext = (window.AudioContext || window.webkitAudioContext);
        if (!AudioContext) {
            return 44100;
        }
        return new AudioContext().sampleRate;
    }());
    (function(DGPlayer) {
        if (!DGPlayer) return;
        playerAPI = DGPlayer;
        DGPlayer.playlist = playlist;
        DGPlayer.volume = 50;

        var player, onplay, onplaylist;
 
        DGPlayer.on('play', onplay = function(a,b,c){
            if (player) {
                player.disconnect();
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(playerAPI.current.url), DGPlayer);
            DGPlayer.off('play', onplay);
        });

        DGPlayer.on('playlist', onplaylist = function(a,b,c) {
            if(player) {
                player.disconnect();
                DGPlayer.state = 'paused';
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(playerAPI.current.url), DGPlayer);
        });
    })(DGPlayer(document.getElementById('dgplayer')));
};


export function trackToggle(trackId, active) {
    if (active) {
        fetchTrack(trackId)
            .then((data) => {
                addTrackToPlaylist(data, trackId)
            })
    } else {
        removeTrackFromPlaylist(trackId);
    }
};

export function albumToggle(albumId, active) {
    if (active) {
        fetchAlbum(albumId)
            .then((data) => {
                data.tracks.forEach((item) => { 
                    addTrackToPlaylist(item.id, data.artist, item.title, item.file, data.album, data.cover);
                });
            });
    } else {
        removeAlbumFromPlaylist(albumId); // TODO: get all tracksId from album and remove them separatly
    }
};

export function randomAdd() {
    fetchRandom()
        .then((data) => {
            data.forEach((item) => {
                addTrackToPlaylist(item.id, item.artist, item.title, item.file, item.album, item.cover);
            });
        });
};


function addTrackToPlaylist(id, artist, title, file, album, cover) {
    playerAPI.addSong = {
        _id    : id,
        name   : artist + ' - ' + title,
        url    : encodeURI(file),
        artist : artist,
        album  : album,
        picture:  encodeURI(cover)
    };
};


function removeTrackFromPlaylist(id) {
    if (!playerAPI) return;
    var songToRemove = playerAPI.playlist.find((song) => song._id === id);
    var indexOfSong = playerAPI.playlist.indexOf(songToRemove);
    if (indexOfSong < 0) {
        console.warn('NOTHING_TO_REMOVE');
        return;
    }
    playerAPI.removeSong = indexOfSong;
};
