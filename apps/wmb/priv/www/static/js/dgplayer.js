    // Test data
    var album = 
    {   "artist":"H. Purcell",
        "album":"Soprano and baroque ensemble",
        "picture":"img/avatar.jpeg",
        "songs":[
            {
                "name": "Test Track 1",
                "url": "/files/Freddie%20Hubbard/1969%20-%20A%20Soul%20Experiment/01%20-%20Clap%20Your%20Hands.flac"
            },
            {
                "name": "Test Track 2",
                "url": "/files/Freddie%20Hubbard/1969%20-%20A%20Soul%20Experiment/08%20-%20Midnite%20Soul.flac",
                "picture": "/files/Freddie%20Hubbard/1969%20-%20A%20Soul%20Experiment/cover.jpg"
            }
        ]
    };
    // Chrome doesn't support changing the sample rate, and uses whatever the hardware supports.
    // We cheat here.  Instead of resampling on the fly, we're currently just loading two different
    // files based on common hardware sample rates.
    var _sampleRate = (function() {
        var AudioContext = (window.AudioContext || window.webkitAudioContext);
        if (!AudioContext)
            return 44100;
        
        return new AudioContext().sampleRate;
    }());
    (function(DGPlayer) {
        var add = document.querySelector('.add-song'), remove = document.querySelector('.remove-song');
        add.onclick = function() {
            DGPlayer.addSong =  {
                "name": "Some Test 1",
                "url": "/files/Zakk%20Wylde/1996%20-%20Book%20Of%20Shadows/06%20-%20What%20You%27re%20Look%27n%20For.flac",
                "picture": "medias/debussy.jpg"
            };
        }
        remove.onclick = function() {
            DGPlayer.removeSong = 2; //dummy example ofc
        }
        DGPlayer.album = album;
            
        DGPlayer.volume = 100;
        var player, onplay;
        
        DGPlayer.on('play', onplay = function(){
            if (player)
                player.disconnect();
                
            player = new DGAuroraPlayer(AV.Player.fromURL(DGPlayer.current.url), DGPlayer);
            DGPlayer.off('play', onplay);
        });
        DGPlayer.on('playlist', onplaylist = function() {
            if(player) {
                player.disconnect();
                DGPlayer.state = 'paused';
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(DGPlayer.current.url), DGPlayer);
        });
    })(DGPlayer(document.getElementById('dgplayer')));

