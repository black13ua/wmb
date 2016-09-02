-module(albums_merger).
-export([get_albums/1, get_albums/2]).

-include("ets_names.hrl").

-define(DEFAULT_ITEMS, 10).


get_albums(Format) ->
    get_albums(Format, ?DEFAULT_ITEMS).

% Format can be json or tpl
get_albums(Format, Items) ->
    FirstAlbumTuple = ets:first(?ETS_ALBUMS),
    case Format of
        json ->
            io:format("JSON Format Selected: ~p~n", [[Format, Items]]); 
        tpl ->
            io:format("TPL Format Selected: ~p~n", [[Format, Items]]),
            Result = get_tpl(FirstAlbumTuple, Items, []),
            Result;
        _ -> 
            io:format("Unknown Format Selected: ~p~n", [[Format, Items]])
    end.


get_tpl(AlbumTuple, 0, ResultAcc) ->
    {ok, ResultFromEts} = ets_lookup_album(AlbumTuple),
    {ok, [ResultFromEts | ResultAcc]};
get_tpl(AlbumTuple, Items, ResultAcc) ->
    {ok, ResultFromEts} = ets_lookup_album(AlbumTuple),
    AlbumTupleNext = ets:next(?ETS_ALBUMS, AlbumTuple),
    get_tpl(AlbumTupleNext, Items - 1, [ResultFromEts | ResultAcc]).

ets_lookup_album(AlbumTuple) ->
    [{AlbumTuple, AlbumID}|_] = ets:lookup(?ETS_ALBUMS, AlbumTuple),
    [{AlbumID, GenreTuple}] = ets:lookup(?ETS_GENRES, AlbumID),
    [{AlbumID, CoverTuple}] = ets:lookup(?ETS_COVERS, AlbumID),
    [{AlbumID, PathTuple}] = ets:lookup(?ETS_PATHS, AlbumID),
    TracksList = ets:match(?ETS_TRACKS, {AlbumID, {'$2', '$1'}}),
    AlbumList = erlang:tuple_to_list(AlbumTuple),
    AlbumResult = [CoverTuple, GenreTuple, PathTuple, {tracks, TracksList} | AlbumList],
    {ok, AlbumResult}.

