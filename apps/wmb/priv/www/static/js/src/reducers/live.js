import Immutable from 'seamless-immutable';
import * as actionTypes from '../../constants/live/live';
// import * as socketActionTypes from '../../constants/common/socket';


const initialState = Immutable(
  {
    data: {
      sportsDataById: {},
      categoriesDataById: {},
      tournamentsDataById: {},
      events: [],
      headMarketsDataById: {},
      outcomesDataById: {},
      oneEvent: {},
    },
    viewState: {
      isFetching: {
        'left-menu': false,
        'one-event': false,
      },
      activeEventId: 0,
      favoriteEvents: [],
      selectedSports: [],
      filters: {
        region: false,
        video: false,
      },
    },
  }
);


export default function liveReducer(state = initialState, action = {}) {
  switch (action.type) {

  case actionTypes.LIVE_FETCHING: {
    const { section, status } = action.payload;
    return state.merge({
      viewState: {
        isFetching: {
          [section]: status,
        },
      },
    }, { deep: true });
  }

  case actionTypes.RECEIVE_LIVE_MENU: {
    const chain = _(action.payload.markets);
    const sortedEventsCollection = getSortedEventsCollection(chain);

    const chainEvents = _(sortedEventsCollection);
    const eventsWithoutHeadMarkets = getEventsWitoutHeadMarkets(chainEvents);
    const headMarketsCollection = getHeadMarketsCollection(chainEvents);

    const chainHeadMarkets = _(headMarketsCollection);
    const normalizedHeadMarkets = getNormalizedHeadMarkets(chainHeadMarkets);
    const normalizedOutcomes = getNormalizedOutcomes(chainHeadMarkets);

    const sportsDataById = getSportsDataById(chainEvents);
    const categoriesDataById = getCategoriesDataById(chainEvents);
    const tournamentsDataById = getTournamentsDataById(chainEvents);

    return state.merge({
      data: {
        sportsDataById,
        categoriesDataById,
        tournamentsDataById,
        events: eventsWithoutHeadMarkets,
        headMarketsDataById: normalizedHeadMarkets,
        outcomesDataById: normalizedOutcomes,
      },
    }, { deep: true });
  }

  case actionTypes.RECEIVE_LIVE_EVENT: {
    const oneEvent = action.payload;

    return state.merge({
      data: {
        oneEvent,
      },
    }, { deep: true });
  }

  case actionTypes.SELECT_LIVE_EVENT: {
    const activeEventId = action.enable ? action.eventId : 0;

    return state.merge({
      viewState: {
        activeEventId,
      },
    }, { deep: true });
  }

  case actionTypes.SELECT_LIVE_SPORT: {
    const selectedSports = state.viewState.selectedSports.asMutable({ deep: true });
    const enable = _.includes(selectedSports, action.sportId);
    const newSelectedSports = enable
      ? _.without(selectedSports, action.sportId)
      : [...selectedSports, action.sportId];

    return state.merge({
      viewState: {
        selectedSports: newSelectedSports,
      },
    }, { deep: true });
  }

  case actionTypes.LIVE_TOGGLE_FAVORITE_EVENT: {
    const favoriteEvents = state.viewState.favoriteEvents.asMutable({ deep: true });
    const enable = _.includes(favoriteEvents, action.eventId);
    const newFavoriteEvents = enable
      ? _.without(favoriteEvents, action.eventId)
      : [...favoriteEvents, action.eventId];

    return state.merge({
      viewState: {
        favoriteEvents: newFavoriteEvents,
      },
    }, { deep: true });
  }

  case actionTypes.LIVE_TOGGLE_REGION_FILTER: {
    const regionFilter = state.viewState.filters.region;
    return state.setIn(['viewState', 'filters', 'region'], !regionFilter);
  }

  case actionTypes.LIVE_TOGGLE_VIDEO_FILTER: {
    const videoFilter = state.viewState.filters.video;
    return state.setIn(['viewState', 'filters', 'video'], !videoFilter);
  }

  default:
    return state;
  }
}


/* Helpers */

function getSortedEventsCollection(chain) {
  return chain
    .uniqBy('sportId')
    .map(sport => _.map(sport.tournaments, (tournament) => ({...tournament, ...sport})))
    .flatMap()
    .map(tournament => _.omit(tournament, 'tournaments'))
    .map(tournament => _.map(tournament.events, (event) => ({...event, ...tournament})))
    .flatMap()
    .map(event => _.omit(event, 'events'))
    .sortBy('sportWeigh', 'categoryWeigh', 'tournamentWeigh', 'eventWeigh', 'eventId')
    .value();
}

function getEventsWitoutHeadMarkets(chainEvents) {
  return chainEvents
    .map(event => ({ ...event, 'headMarketId': event.headMarket.marketId }))
    .map(event => _.omit(event, 'headMarket'))
    .value();
}

function getHeadMarketsCollection(chainEvents) {
  return chainEvents
    .filter(event => !_.isEmpty(event.headMarket))
    .flatMap(event => event.headMarket)
    .value();
}

function getNormalizedHeadMarkets(chainHeadMarkets) {
  return chainHeadMarkets
    .map(headMarket => ({ ...headMarket, 'outcomeIds': _.map(headMarket.outcomes, outcome => outcome.outcomeId)}))
    .map(headMarket => _.omit(headMarket, 'outcomes'))
    .reduce((acum, headMarket) => {
      return { ...acum, [headMarket.marketId]: headMarket }; // eslint-disable-line no-param-reassign
    }, {});
}

function getNormalizedOutcomes(chainHeadMarkets) {
  return chainHeadMarkets
    .flatMap(headMarket => headMarket.outcomes)
    .compact()
    .reduce((acum, outcome) => {
      return { ...acum, [outcome.outcomeId]: outcome }; // eslint-disable-line no-param-reassign
    }, {});
}

function getSportsDataById(chainEvents) {
  return chainEvents
    .uniqBy('sportId')
    .map(val => _.pick(val, ['sportId', 'sportName', 'sportformId', 'countryId']))
    .reduce((acum, val) => {
      return { ...acum, [val.sportId]: val }; // eslint-disable-line no-param-reassign
    }, {});
}

function getCategoriesDataById(chainEvents) {
  return chainEvents
    .uniqBy('categoryId')
    .map(val => _.pick(val, ['sportId', 'categoryId', 'categoryName', 'countryId']))
    .reduce((acum, val) => {
      return { ...acum, [val.categoryId]: val };
    }, {});
}

function getTournamentsDataById(chainEvents) {
  return chainEvents
    .uniqBy('tournamentId')
    .map(val => _.pick(val, ['sportId', 'tournamentId', 'tournamentName', 'categoryName', 'sportName']))
    .reduce((acum, val) => {
      return { ...acum, [val.tournamentId]: val }; // eslint-disable-line no-param-reassign
    }, {});
}
