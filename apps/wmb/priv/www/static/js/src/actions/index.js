import actionCreator from '../utils/actionCreatorFactory';
import * as ActionTypes from '../constants/action-types';

export const receiveABCfilter = () => actionCreator(ActionTypes.RECEIVE_ABC_FILTER);
export const fetchABCfilter = () => actionCreator(ActionTypes.FETCH_ABC_FILTER);
export const fetching = value => actionCreator(ActionTypes.FETCHING, { value });
// export const toggleBoolField  = key => actionCreator(ActionTypes.TOGGLE_BOOL_FIELD, { key });
// export const setFieldError    = (key, value) => actionCreator(ActionTypes.SET_FIELD_ERROR, { key, value });
