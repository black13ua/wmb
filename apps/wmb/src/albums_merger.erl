-module(albums_merger).
-export([get_albums/1, get_albums/2]).


-define(DEFAULT_ITEMS, 10).
-define(ETS_ALBUMS,    wmb_albums).
-define(ETS_GENRES,    wmb_genres).
-define(ETS_PATHS,     wmb_paths).
-define(ETS_TRACKS,    wmb_tracks).


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
    AlbumTupleNext = ets:next(?ETS_ALBUMS, AlbumTuple),
    {ok, ResultFromEts} = ets_lookup_album(AlbumTupleNext),
    get_tpl(AlbumTupleNext, Items - 1, [ResultFromEts | ResultAcc]).

ets_lookup_album(AlbumTuple) ->
    [{AlbumTuple, AlbumID}|_] = ets:lookup(?ETS_ALBUMS, AlbumTuple),
    [{AlbumID, GenreTuple}] = ets:lookup(?ETS_GENRES, AlbumID),
    [{AlbumID, AlbumPath}] = ets:lookup(?ETS_PATHS, AlbumID),
    TracksList = ets:match(?ETS_TRACKS, {AlbumID, {'$2', '$1'}}),
    AlbumList = erlang:tuple_to_list(AlbumTuple),
    {ok, AlbumCover} = album_cover_finder(AlbumPath),
    Res1 = [GenreTuple|AlbumList],
    AlbumResult = [{tracks, TracksList} | Res1],
    {ok, AlbumResult ++ {cover, AlbumCover}}.

album_cover_finder(AlbumPath) ->
    {path, AlbumPathString} = AlbumPath,
    AlbumCover = filelib:wildcard(lists:concat([AlbumPathString, "/", "cover.{jpg,JPG,jpeg,JPEG,png,PNG}"])),
    case AlbumCover of
        [] ->
            io:format("AlbumCover: ~p~n", [[AlbumPathString, not_found]]),
            {ok, not_found};
        _ ->
            io:format("AlbumCover: ~p~n", [AlbumCover]),
            {ok, AlbumCover}
    end.

