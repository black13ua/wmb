var playerAPI = {};
var pathToAlbumsApi = '/api/albums/';
var pathToRandomApi = '/api/random/';
var pathToTracksApi = '/api/tracks/';
var playlist = [];

// DGPlayer Playlist Example
//var playlist = [
//    {
//        name : "Message In a Bottle",
//        url : "/files/Ulf%20Wakenius/2012%20-%20Vagabond/02.%20Message%20In%20a%20Bottle.flac",
//        artist : 'Ulf Wakenius', 
//        album : 'Vagabond', 
//        picture : '/files/Ulf Wakenius/2012 - Vagabond/cover.jpg', 
//    }
//];

document.addEventListener('click', function(event) { 
    reactingOnClicks(event); 
}); 
 
function reactingOnClicks(event) { 
    var target = event.target; 
    var id = +target.dataset.id;
    var already = handleActiveClass(target);
    var mainClassName = target.className.replace(/active/gi, '').trim(); 
    switch(mainClassName) { 
        case 'add-album': {
            var fullPath = pathToAlbumsApi + id;
            console.info(already, fullPath, target);
            addOrRemoveToPlaylist(id, already, fullPath);
            break;
        }
        case 'add-random': {
            var fullPath = pathToRandomApi + id;
            console.info(already, fullPath);
            addOrRemoveToPlaylist(id, already, fullPath);
            break;
        }
        case 'add-track': {
            var fullPath = pathToTracksApi + id;
            console.info(already, fullPath);
            addOrRemoveToPlaylist(id, already, fullPath);
            break;
        }
    } 
}; 

function handleActiveClass(target) { 
    var isAlreadyActive = Array.prototype.join.call(target.classList, '').match(/active/gi); 
    if (isAlreadyActive) { 
        target.classList.remove('active'); 
        return false;
    } else { 
        target.classList.add('active');
        return true;
    } 
};

function addOrRemoveToPlaylist(id, active, path) {
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
        _id: id,
        name: artist + ' - ' + title,
        url: encodeURI(file),
        artist: artist,
        album: album,
        picture: cover
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

//
// Player
//
function createPlayer(playlist) {
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
        var player, onplay;
 
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

//
// For Moving Player
//
function adddragAbilityToPlayer() {
    document.getElementById('dgplayer').addEventListener('mousedown', mouseDown, false);
    window.addEventListener('mouseup', mouseUp, false);
    var div = document.getElementById('dgplayer-item');
    var diff = {};
    function mouseUp() {
        window.removeEventListener('mousemove', divMove, true);
    }

    function mouseDown(e) {
        diff = {
            x: e.clientX - div.offsetLeft,
            y: e.clientY - div.offsetTop
        };
        window.addEventListener('mousemove', divMove, true);
        console.warn('about to move', diff);
    }

    function divMove(e){
      console.info(e.clientY, e.clientX);
      div.style.position = 'fixed';
      div.style.top = e.clientY - diff.x + 'px';
      div.style.left = e.clientX - diff.y + 'px';
    }
};

document.addEventListener("DOMContentLoaded", function(event) {
    createPlayer(playlist);
//    adddragAbilityToPlayer();
});

