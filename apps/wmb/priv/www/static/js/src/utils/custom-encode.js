export function customEncode(url) {
    const defaultEncode = window.encodeURI(url);
    return defaultEncode
        .replace('?', '%3F')
        .replace('#', '%23')
        .replace(/\*/g, '%2A');
}
