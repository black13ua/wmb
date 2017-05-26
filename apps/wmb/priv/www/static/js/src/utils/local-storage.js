import { isNil } from 'lodash';


export function getVolumeFromLocalStorage() {
    return JSON.parse(localStorage.getItem('auroraPlayer.userVolume'));
}

export function setVolumeToLocalStorage(volume) {
    if (isNil(volume)) return;
    localStorage.setItem('auroraPlayer.userVolume', JSON.stringify(volume));
}
