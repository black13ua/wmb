/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// identity function for calling harmony imports with the correct context
/******/ 	__webpack_require__.i = function(value) { return value; };
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 3);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
var ALBUMS_URL = exports.ALBUMS_URL = '/api/albums/';
var RANDOM_URL = exports.RANDOM_URL = '/api/random/';
var TRACKS_URL = exports.TRACKS_URL = '/api/tracks/';

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.createPlayer = createPlayer;
exports.addOrRemoveToPlaylist = addOrRemoveToPlaylist;
var playerAPI = {};

function createPlayer() {
    var playlist = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : [];

    var _sampleRate = function () {
        var AudioContext = window.AudioContext || window.webkitAudioContext;
        if (!AudioContext) {
            return 44100;
        }
        return new AudioContext().sampleRate;
    }();
    (function (DGPlayer) {
        if (!DGPlayer) return;
        playerAPI = DGPlayer;
        DGPlayer.playlist = playlist;
        DGPlayer.volume = 50;

        var player, _onplay, onplaylist;

        DGPlayer.on('play', _onplay = function onplay(a, b, c) {
            if (player) {
                player.disconnect();
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(playerAPI.current.url), DGPlayer);
            DGPlayer.off('play', _onplay);
        });

        DGPlayer.on('playlist', onplaylist = function onplaylist(a, b, c) {
            if (player) {
                player.disconnect();
                DGPlayer.state = 'paused';
            }
            player = new DGAuroraPlayer(AV.Player.fromURL(playerAPI.current.url), DGPlayer);
        });
    })(DGPlayer(document.getElementById('dgplayer')));
};

function addOrRemoveToPlaylist(id, active, path) {
    if (active) {
        addToPlaylist(path, id);
    } else {
        console.info(id, active, path);
        removeTrackFromPlaylist(id);
    }
};

function addToPlaylist(fullPath, id) {
    console.info(fullPath);
    $.get(fullPath).done(function (data) {
        if (!playerAPI) return;
        // Random Tracks List
        if (Array.isArray(data)) {
            data.forEach(function (item) {
                addTrackToPlaylist(id, item.artist, item.title, item.file, item.album, item.cover);
            });
        }
        // Album
        else if (data.tracks) {
                data.tracks.forEach(function (item) {
                    addTrackToPlaylist(id, data.artist, item.title, item.file, data.album, data.cover);
                });
                // Track
            } else {
                addTrackToPlaylist(id, data.artist, data.title, data.file, data.album, data.cover);
            }
    }).fail(function (error) {
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
        picture: encodeURI(cover)
    };
};

function removeTrackFromPlaylist(id) {
    if (!playerAPI) return;
    var songToRemove = playerAPI.playlist.find(function (song) {
        return song._id === id;
    });
    var indexOfSong = playerAPI.playlist.indexOf(songToRemove);
    if (indexOfSong < 0) {
        console.warn('NOTHING_TO_REMOVE');
        return;
    }
    playerAPI.removeSong = indexOfSong;
};

/***/ }),
/* 2 */
/***/ (function(module, exports) {

// removed by extract-text-webpack-plugin

/***/ }),
/* 3 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


__webpack_require__(2);

var _constants = __webpack_require__(0);

var _player = __webpack_require__(1);

function reactingOnClicks(event) {
    var target = event.target;
    var id = +target.dataset.id;
    var already = handleActiveClass(target);
    var mainClassName = target.className.replace(/active/gi, '').trim();
    switch (mainClassName) {
        case 'add-album':
            {
                var fullPath = _constants.pathToAlbumsApi + id;
                console.info(already, fullPath, target);
                (0, _player.addOrRemoveToPlaylist)(id, already, fullPath);
                break;
            }
        case 'add-random':
            {
                var fullPath = _constants.pathToRandomApi + id;
                console.info(already, fullPath);
                (0, _player.addOrRemoveToPlaylist)(id, already, fullPath);
                break;
            }
        case 'add-track':
            {
                var fullPath = _constants.pathToTracksApi + id;
                console.info(already, fullPath);
                (0, _player.addOrRemoveToPlaylist)(id, already, fullPath);
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

document.addEventListener("DOMContentLoaded", function (event) {
    (0, _player.createPlayer)();

    document.addEventListener('click', function (event) {
        reactingOnClicks(event);
    });
});

/***/ })
/******/ ]);
//# sourceMappingURL=bundle.js.map