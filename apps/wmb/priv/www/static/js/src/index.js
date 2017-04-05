import 'babel-polyfill';

import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import App from '../../containers/common/App';
// import DevTools from './containers/DevTools';
import configureStore from '../../store/live/live-configure-store';
import RootTime from '../../clock/rootclock';
import ServiceMenu from '../../containers/common/servicemenu';
import JackpotContainer from '../../containers/common/jackpot';
import LiveLeftMenuContainer from '../../containers/live/left-menu/left-menu-container';
import LiveCenterContainer from '../../containers/live/center/center-container';
import { DATA_URL_BASKET_LIVE } from '../../constants/live/live';

const initialState = {};
const store = configureStore(initialState);

ReactDOM.render(
  <Provider store={ store }>
    <App />
  </Provider>,
  document.getElementById('sright')
);

if (document.getElementById('times_place')) {
  ReactDOM.render(
    <RootTime />,
    document.getElementById('times_place')
  );
}

if (document.getElementById('puthere')) {
  ReactDOM.render(
    <Provider store={ store }>
      <LiveLeftMenuContainer />
    </Provider>,
    document.getElementById('puthere')
  );
}

if (document.getElementById('livediv')) {
  ReactDOM.render(
    <Provider store={ store }>
      <LiveCenterContainer />
    </Provider>,
    document.getElementById('livediv')
  );
}

if (document.getElementById('servicemenu')) {
  ReactDOM.render(
    <Provider store={ store }>
      <ServiceMenu />
    </Provider>,
    document.getElementById('servicemenu')
  );
}

if (document.getElementById('jackpot')) {
  ReactDOM.render(
    <Provider store={ store }>
      <JackpotContainer />
    </Provider>,
    document.getElementById('jackpot')
  );
}
