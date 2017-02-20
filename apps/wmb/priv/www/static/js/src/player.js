let playerAPI = {};

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


export function addOrRemoveToPlaylist(id, active, path) {
    if (active) {
        addToPlaylist(path, id)
    } else {
        console.info(id, active, path);
        removeTrackFromPlaylist(id);
    }
};


function addToPlaylist(fullPath, id) {
    console.info(fullPath);
    $.get(fullPath)
        .done(function(data) {
            if (!playerAPI) return;
            // Random Tracks List
            if (Array.isArray(data)) {
                data.forEach(function(item) {
                    addTrackToPlaylist(id, item.artist, item.title, item.file, item.album, item.cover);
                });
            }
            // Album
            else if (data.tracks) {
                data.tracks.forEach(function(item) {
                    addTrackToPlaylist(id, data.artist, item.title, item.file, data.album, data.cover);
                });
            // Track
            } else {
                addTrackToPlaylist(id, data.artist, data.title, data.file, data.album, data.cover)
            }
        })
        .fail(function(error) {
            console.warn('NO_ALBUMS');
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
    var songToRemove = playerAPI.playlist.find(function(song){ return song._id === id});
    var indexOfSong = playerAPI.playlist.indexOf(songToRemove);
    if (indexOfSong < 0) {
        console.warn('NOTHING_TO_REMOVE');
        return;
    }
    playerAPI.removeSong = indexOfSong;
};
