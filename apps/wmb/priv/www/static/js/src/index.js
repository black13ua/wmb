import '../../sass/main.scss';
import {
    createPlayer,
    randomAdd,
    trackToggle,
    albumToggle
} from './player';

function reactingOnClicks(event) { 
    var target = event.target; 
    var id = +target.dataset.id;
    var already = handleActiveClass(target);
    var mainClassName = target.className.replace(/active/gi, '').trim(); 
    switch(mainClassName) { 
        case 'add-album': {
            albumToggle(albumId, already);
            break;
        }
        case 'add-random': {
            randomAdd();
            break;
        }
        case 'add-track': {
            trackToggle(trackId, already);
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


document.addEventListener("DOMContentLoaded", function(event) {
    createPlayer();

    document.addEventListener('click', function(event) { 
        reactingOnClicks(event); 
    });
});
