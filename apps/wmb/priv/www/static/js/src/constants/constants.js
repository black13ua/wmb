export const RANDOM_NUMBER = 15;

// extra load will start when this track (number from the end of the playlist) starts to play
export const TRACK_POINT_TO_EXTRA_LOAD = 1; // 1 - last track in the playlist
export const PLAYER_HTML = `
    <div class="player-container">
        <div class="player" id="dgplayer" tabindex="0">
            <div id="dgplayer-item" class="cf player-item">
                <div class="avatar-container">
                    <div class="avatar">
                        <img src="/static/dgplayer/img/avatar.jpeg">
                    </div>
                </div>
                <div class="info-container">
                    <span class="title">Unknown Title</span>
                    <span class="artist">Unknown Artist</span>
                    <div class="seek">
                        <span>0:00</span>
                        <div class="track">
                            <div class="loaded"></div>
                            <div class="progress"></div>
                        </div>
                        <span>-0:00</span>
                    </div>
                </div>
                <div class="controls active">
                    <div class="buttons">
                        <div class="button-prev"><i class="entypo-fast-backward"></i></div>
                        <div class="button"><i class="entypo-play"></i></div>
                        <div class="button-next"><i class="entypo-fast-forward"></i></div>
                    </div>
                    <div class="volume">
                    <img src="/static/dgplayer/img/volume_low.png">
                        <div class="track">
                            <div class="progress" style="height: 60px; width: 25px;"></div>
                            <div class="handle" style="transform: translate3d(25px, 0px, 0px);"></div>
                        </div>
                        <img src="/static/dgplayer/img/volume_high.png">
                    </div>
                    <div class="button-playlist"><i class="entypo-numbered-list"></i></div>
                </div>
            </div>
        </div>
    </div>
`;
