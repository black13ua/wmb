import { takeEvery } from 'redux-saga';
import { put, call, fork } from 'redux-saga/effects';

import {
  fetchLiveMenu,
  fetchLiveEvent,
} from '../../api/live/live.js';

import {
  FETCH_LIVE_MENU,
  FETCH_LIVE_EVENT,
  RECEIVE_LIVE_MENU,
  RECEIVE_LIVE_EVENT,
  SELECT_LIVE_EVENT,
  LIVE_FETCHING,
} from '../../constants/live/live.js';

import {
  SET_LIVE_SUBSCRIPTION,
  CHANGE_SERVICES_SUBSCRIPTION,
} from '../../constants/common/socket.js';


function getLiveMenu() {
  return fetchLiveMenu()
    .then(response => ({ response }))
    .catch(error => ({ error }));
}

function getLiveEvent(eventId) {
  return fetchLiveEvent(eventId)
    .then(response => ({ response }))
    .catch(error => ({ error }));
}

function* setLiveMenu(response, error) {
  if (response) {
    yield put({ type: RECEIVE_LIVE_MENU, payload: response });
    yield put({ type: LIVE_FETCHING, payload: { section: 'left-menu', status: false } });
    yield put({ type: SET_LIVE_SUBSCRIPTION, eventId: null }); // set/change store subscription
    yield put({ type: CHANGE_SERVICES_SUBSCRIPTION }); // make socket saga reinit subscription
  }
  // Пока что не обрабатываем ошибки получения
  else yield put({ type: 'ERROR_LIVE_MENU', error });
}

function* setLiveEvent(response, error, eventId) {
  if (response) {
    yield put({ type: RECEIVE_LIVE_EVENT, payload: response });
    yield put({ type: LIVE_FETCHING, payload: { section: 'one-event', status: false } });
    yield put({ type: SELECT_LIVE_EVENT, eventId }); // set in left menu after receiving
    yield put({ type: SET_LIVE_SUBSCRIPTION, eventId }); // set/change store subscription
    yield put({ type: CHANGE_SERVICES_SUBSCRIPTION }); // make socket saga reinit subscription
  }
  // Пока что не обрабатываем ошибки получения
  else yield put({ type: 'ERROR_LIVE_EVENT', error });
}


/* === WORKERS === */
export function* getLiveMenuWorker() {
  yield put({ type: LIVE_FETCHING, payload: { section: 'left-menu', status: true } });
  const { response, error } = yield call(getLiveMenu);
  yield call(setLiveMenu, response, error);
}

export function* getLiveEventWorker(action) {
  const { eventId } = action;
  yield put({ type: LIVE_FETCHING, payload: { section: 'one-event', status: true } });
  const { response, error } = yield call(getLiveEvent, eventId);
  yield call(setLiveEvent, response, error, eventId);
}


/* WATHERS */
function* liveMenuWatcher() {
  yield takeEvery(FETCH_LIVE_MENU, getLiveMenuWorker);
}

function* liveEventWatcher() {
  yield takeEvery(FETCH_LIVE_EVENT, getLiveEventWorker);
}


/* === ROOT === */
export default function* liveSaga() {
  yield [
    fork(liveMenuWatcher),
    fork(liveEventWatcher),
  ];
}
