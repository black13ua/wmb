export default (type, payload) => (payload ? { type, payload } : { type });
