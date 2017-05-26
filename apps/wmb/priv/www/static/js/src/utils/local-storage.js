export function getVolumeFromLocalStorage() {
    return JSON.parse(localStorage.getItem('auroraPlayer.userVolume'));
}

export function setVolumeToLocalStorage(volume) {
    localStorage.setItem('auroraPlayer.userVolume', JSON.stringify(volume));
}
