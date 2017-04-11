const configureStore = __DEVELOPMENT__
    ? require('./configureStore.dev').default
    : require('./configureStore.prod').default;

const store = configureStore();

export default store;
