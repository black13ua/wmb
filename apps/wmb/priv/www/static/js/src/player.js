import humps from 'humps';
import { fetchAlbum, fetchTrack, fetchRandom } from './api';


let playerAPI = {};

/* eslint-disable */
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
/* eslint-enable */

export function trackToggle(trackId, active) {
    if (active) {
        fetchTrack(trackId)
            .then((json) => {
                addTrackToPlaylist(json);
            });
    } else {
        removeTrackFromPlaylist(trackId);
    }
}

export function albumToggle(albumId, active) {
    if (active) {
        fetchAlbum(albumId)
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
    fetchRandom()
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
    console.info('addTrackToPlaylist', trackId, artist, title, file, cover, album);
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
