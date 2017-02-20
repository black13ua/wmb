require('../../sass/main.scss');
import { pathToAlbumsApi, pathToRandomApi, pathToTracksApi } from './constants';
import { 
    createPlayer,
    addOrRemoveToPlaylist,
} from './player';

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


document.addEventListener("DOMContentLoaded", function(event) {
    createPlayer();

    document.addEventListener('click', function(event) { 
        reactingOnClicks(event); 
    });
});
