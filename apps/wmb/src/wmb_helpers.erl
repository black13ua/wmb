-module(wmb_helpers).
-export([ceiling/1, get_uniq_id/1, skip_ets_elements/2]).

-include("ets_names.hrl").


%%%%
% https://erlangcentral.org/wiki/index.php/Floating_Point_Rounding
%%%%
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

skip_ets_elements(0, Ets) ->
    Key = ets:first(Ets),
    Key;
skip_ets_elements(Skip, Ets) ->
    Key = ets:first(Ets),
    skip_ets_elements(Skip, Ets, Key).

skip_ets_elements(0, Ets, Key) ->
    Key;
skip_ets_elements(Skip, Ets, Key) ->
    Next = ets:next(Ets, Key),
    skip_ets_elements(Skip - 1, Ets, Next).

-spec get_uniq_id(atom()) ->
    {ok, integer()}.
get_uniq_id(Key) ->
    ID = ets:lookup_element(?ETS_COUNTERS, Key, 2),
    NewID = ID + 1,
    ets:update_element(?ETS_COUNTERS, Key, {2, NewID}),
    {ok, NewID}.
