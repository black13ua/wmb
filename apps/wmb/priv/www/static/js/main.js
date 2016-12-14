console.info('Hello once again!');
var playerAPI = {};

var playlist = [
    {
        name : "Message In a Bottle",
        url : "/files/Ulf%20Wakenius/2012%20-%20Vagabond/02.%20Message%20In%20a%20Bottle.flac",
        artist : 'Ulf Wakenius', 
        album : 'Vagabond', 
        picture : '/files/Ulf Wakenius/2012 - Vagabond/cover.jpg', 
    }
];

function addTrackToPlaylist(name, url, artist, album, picture) {
    if (!playerAPI) return;
    var songObj = {
        name: name,
        url: url,
        artist: artist,
        album: album,
        picture: picture
    };
    playerAPI.addSong = songObj;
}

function removeTrackFromPlaylist(url) {
    if (!playerAPI) return;
    var songToRemove = playerAPI.playlist.find(function(song){ return song.url === url});
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
    var artist = target.dataset.artist;
    var name = target.dataset.name;
    var picture = target.dataset.picture;
    var album = target.dataset.album;
    var url = target.dataset.url;
    if (isAlreadyActive) { 
        target.classList.remove('active'); 
        removeTrackFromPlaylist(url);
    } else { 
        target.classList.add('active'); 
        addTrackToPlaylist(name, url, artist, album, picture);
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
