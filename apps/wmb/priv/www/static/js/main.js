console.info('Hello once again!');
var playerAPI = {};
var pathToFilesApi = '/api/tracks/'
var playlist = [
    {
        name : "Message In a Bottle",
        url : "/files/Ulf%20Wakenius/2012%20-%20Vagabond/02.%20Message%20In%20a%20Bottle.flac",
        artist : 'Ulf Wakenius', 
        album : 'Vagabond', 
        picture : '/files/Ulf Wakenius/2012 - Vagabond/cover.jpg', 
    }
];

function addTrackToPlaylist(id) {
    var fullPath = pathToFilesApi + id;
    $.get(fullPath)
        .done(function(data) {
            if (!playerAPI) return;
            playerAPI.addSong = {
                _id: id,
                name: data.artist + ' - ' + data.title,
                url: encodeURI(data.file),
                artist: data.artist,
                album: data.album,
                picture: data.cover
            };
        })
        .fail(function(error) {
            console.warn('NO_TRACK');
        });
}

function removeTrackFromPlaylist(id) {
    if (!playerAPI) return;
    var songToRemove = playerAPI.playlist.find(function(song){ return song._id === id});
    var indexOfSong = playerAPI.playlist.indexOf(songToRemove);
    if (indexOfSong < 0) {
        console.warn('NOTHING_TO_REMOVE');
        return;
    }
    playerAPI.removeSong = indexOfSong;
}

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

document.addEventListener('click', function(event) { 
    reactingOnClicks(event); 
}); 
 
function reactingOnClicks(event) { 
    var target = event.target; 
    var mainClassName = target.className.replace(/active/gi, '').trim(); 
    switch(mainClassName) { 
        case 'add-button': {
            handleActiveClass(target); 
            break;
        }
    } 
} 

function handleActiveClass(target) { 
    var isAlreadyActive = Array.prototype.join.call(target.classList, '').match(/active/gi); 
    var id = +target.dataset.id;
    if (isAlreadyActive) { 
        target.classList.remove('active'); 
        removeTrackFromPlaylist(id);
    } else { 
        target.classList.add('active'); 
        addTrackToPlaylist(id);
    } 
};

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
      console.info(e.clientY, clientX);
      div.style.position = 'fixed';
      div.style.top = e.clientY - diff.x + 'px';
      div.style.left = e.clientX - diff.y + 'px';
    }
};

document.addEventListener("DOMContentLoaded", function(event) {
    createPlayer(playlist);
    // adddragAbilityToPlayer();
});
