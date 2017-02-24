import '../../sass/main.scss';
import {
    createPlayer,
    randomAdd,
    trackToggle,
    albumToggle
} from './player';

function reactingOnClicks(event) {
    const target = event.target;
    const id = +target.dataset.id;
    const already = handleActiveClass(target);
    const mainClassName = target.className.replace(/active/gi, '').trim();
    switch (mainClassName) {
        case 'add-album': {
            albumToggle(id, already);
            break;
        }
        case 'add-random': {
            randomAdd();
            break;
        }
        case 'add-track': {
            trackToggle(id, already);
            break;
        }
        default: {
            return null;
        }
    }
}

function handleActiveClass(target) {
    const isAlreadyActive = Array.prototype.join.call(target.classList, '').match(/active/gi);
    let already = false;
    if (isAlreadyActive) {
        target.classList.remove('active');
        already = false;
    } else {
        target.classList.add('active');
        already = true;
    }
    return already;
}


document.addEventListener('DOMContentLoaded', () => {
    createPlayer();

    document.addEventListener('click', (event) => {
        reactingOnClicks(event);
    });
});
