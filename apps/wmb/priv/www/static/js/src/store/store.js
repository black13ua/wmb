const configureStore = __DEVELOPMENT__
    ? require('./configureStore.dev.js').default
    : require('./configureStore.prod.js').default;

const store = configureStore();

export default store;
