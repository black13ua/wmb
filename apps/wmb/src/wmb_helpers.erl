-module(wmb_helpers).
-export([ceiling/1, skip_ets_elements/2]).


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
    case ets:next(Ets, Key) of
        '$end_of_table' ->
            Key;
        Next ->
            skip_ets_elements(Skip - 1, Ets, Next)
    end.


%    First = ets:first(Ets),
