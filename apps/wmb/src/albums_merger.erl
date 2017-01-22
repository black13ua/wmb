-module(albums_merger).
-export([get_albums/1, get_albums/3, get_tracklist_by_albumid/1, get_tracklist_by_albumtuple/1]).

-include("ets_names.hrl").

-define(DEFAULT_ITEMS, 10).
-define(DEFAULT_SKIP,  1).


get_albums(Format) ->
    get_albums(Format, ?DEFAULT_SKIP, ?DEFAULT_ITEMS).

% Format can be json or tpl
get_albums(Format, Skip, Items) ->
    FirstAlbumTuple = wmb_helpers:skip_ets_elements(Skip, ?ETS_ALBUMS),
    case Format of
        json ->
            io:format("JSON Format Selected: ~p~n", [[Format, Items]]); 
        tpl ->
            io:format("TPL Format Selected: ~p~n", [[Format, Skip, Items]]),
            Result = get_tpl(FirstAlbumTuple, Items, []),
            Result;
        _ -> 
            io:format("Unknown Format Selected: ~p~n", [[Format, Items]])
    end.


get_tpl(AlbumTuple, 1, ResultAcc) ->
    {ok, ResultFromEts} = ets_lookup_album(AlbumTuple),
    {ok, lists:reverse([ResultFromEts | ResultAcc])};
get_tpl(AlbumTuple, Items, ResultAcc) ->
    {ok, ResultFromEts} = ets_lookup_album(AlbumTuple),
    case ets:next(?ETS_ALBUMS, AlbumTuple) of
        '$end_of_table' ->
            {ok, lists:reverse([ResultFromEts | ResultAcc])};
        AlbumTupleNext ->
            get_tpl(AlbumTupleNext, Items - 1, [ResultFromEts | ResultAcc])
    end.

ets_lookup_album(AlbumTuple) ->
    [{AlbumTuple, AlbumID}|_] = ets:lookup(?ETS_ALBUMS, AlbumTuple),
    [{AlbumID, AlbumArtist}] = ets:lookup(?ETS_ARTISTS, AlbumID),
    [{AlbumID, CoverTuple}] = ets:lookup(?ETS_COVERS, AlbumID),
    [{AlbumID, GenreTuple}] = ets:lookup(?ETS_GENRES, AlbumID),
    [{AlbumID, PathTuple}] = ets:lookup(?ETS_PATHS, AlbumID),
%    TracksList = ets:match(?ETS_TRACKS, {AlbumID, {'$2', '$1', '$3'}}),
io:format("AlbumID: ~p~n", [AlbumID]),
    {ok, TracksList} = get_tracklist_by_albumtuple(AlbumID),
    AlbumList = erlang:tuple_to_list(AlbumTuple),
    AlbumResult = [AlbumID, AlbumArtist, CoverTuple, GenreTuple, PathTuple, {tracks, TracksList} | AlbumList],
    {ok, AlbumResult}.

get_tracklist_by_albumid(AlbumID) ->
    get_tracklist_by_albumtuple({album_id, AlbumID}).

get_tracklist_by_albumtuple(AlbumID) ->
    FilesUrlRoot = <<"/files/">>,
    [{AlbumID, {path, AlbumPathBin}}] = ets:lookup(?ETS_PATHS, AlbumID),
    TracksList = ets:match(?ETS_TRACKS, {AlbumID, {'$2', '$1', '$3'}}),
    case TracksList of
        [] ->
            {error, trackslist_empty};
        _ ->
            TracksListWithPath = lists:map(fun(X) ->
                                 File = proplists:get_value(file, X),
                                 Title = proplists:get_value(title, X),
                                 TrackID = proplists:get_value(track_id, X),
                                 FullPath = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, File/binary>>,
                                 [{file, FullPath}, {title, Title}, {track_id, TrackID}]  end, TracksList),
            {ok, TracksListWithPath}
    end.

