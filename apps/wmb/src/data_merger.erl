-module(data_merger).
-export([
    ets_lookup_album/1,
    get_albums/1, get_albums/3,
    get_tracklist_by_albumid/1, get_tracklist_by_albumtuple/1,
    get_track_by_trackid/1,
    get_albums_by_genre_name/1, get_albums_by_genre_tuple/1,
    get_albumtuple_by_albumid/1,
    get_random_tracks/1
]).

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
    io:format("AlbumID is: ~p~n", [AlbumID]),
    {ok, TracksList} = get_tracklist_by_albumtuple(AlbumID),
    AlbumList = erlang:tuple_to_list(AlbumTuple),
    AlbumResult = [AlbumID, AlbumArtist, CoverTuple, GenreTuple, PathTuple, {tracks, TracksList} | AlbumList],
    {ok, AlbumResult}.

-spec get_tracklist_by_albumid(integer()) ->
    {ok, []} | {ok, [proplists:proplist()]} | {error, atom()}.
get_tracklist_by_albumid(AlbumID) ->
    get_tracklist_by_albumtuple({album_id, AlbumID}).

-spec get_tracklist_by_albumtuple({album_id, integer()}) ->
    {ok, []} | {ok, [proplists:proplist()]} | {error, atom()}.
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

%%% Get AlbumTuple by AlbumID
-spec get_albumtuple_by_albumid({album_id, integer()}) ->
    {ok, {{album, bitstring(), bitstring()}}}.
get_albumtuple_by_albumid(AlbumID) ->
    [[AlbumTuple]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
    io:format("AlbumTuple is: ~p~n", [AlbumTuple]),
    {ok, AlbumTuple}.

%%% Get AlbumList by Genre Name
-spec get_albums_by_genre_name(bitstring()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_genre_name(GenreName) ->
    get_albums_by_genre_tuple({genre, GenreName}).

%%% Get AlbumList by Genre Tuple
-spec get_albums_by_genre_tuple({genre, bitstring()}) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_genre_tuple(GenreTuple) ->
    AlbumIDList = ets:match(?ETS_GENRES, {'$1', GenreTuple}),
    get_albums_by_albumtuplelist(AlbumIDList, []).

-spec get_albums_by_albumtuplelist([proplists:proplist()], list()) ->
    {ok, [proplists:proplist()]}.
get_albums_by_albumtuplelist([[AlbumID]|Rest], Acc) ->
    {ok, AlbumTuple} = get_albumtuple_by_albumid(AlbumID),
    {ok, R} = ets_lookup_album(AlbumTuple),
    get_albums_by_albumtuplelist(Rest, [R|Acc]);
get_albums_by_albumtuplelist([], Acc) ->
    {ok, Acc}.

%%% Get TrackList with N Tracks
%%% > erlang:element(2, {a, b, c}).
%%% b
%%% > erlang:length([1,2,3,4,5,6,7,8,9]).
%%% 9
-spec get_random_tracks(integer()) ->
    {ok, [proplists:proplist()]}.
get_random_tracks(N) ->
    AlbumsCount = ets:info(?ETS_ALBUMS, size),
    TracksCount = ets:info(?ETS_TRACKS, size),
    {ok, TracksListRandom} = get_random_tracks(N, crypto:rand_uniform(1, AlbumsCount + TracksCount), AlbumsCount + TracksCount, []),
    {ok, TracksListRandom}.

get_random_tracks(0, RandomID, MaxID, Acc) ->
    {ok, Acc};
get_random_tracks(N, RandomID, MaxID, Acc) ->
    RandomTrack = ets:match(?ETS_TRACKS, {'_', {'_', '_', {track_id, RandomID}}}),
    case RandomTrack of
        [] ->
            get_random_tracks(N, crypto:rand_uniform(1, MaxID), MaxID, Acc); 
        _ ->
            {ok, TrackJson} = get_track_by_trackid({track_id, RandomID}),
            io:format("RandomID: ~p~n", [[RandomID, ?MODULE]]),
            get_random_tracks(N - 1, crypto:rand_uniform(1, MaxID), MaxID, [TrackJson|Acc])
    end.

get_track_by_trackid(TrackID) ->
    FilesUrlRoot = <<"/files/">>,
    [[AlbumID, {file, File}, Title]] = ets:match(?ETS_TRACKS, {'$1', {'$2', '$3', TrackID}}),
    [{AlbumID, {cover, AlbumCover}}] = ets:lookup(?ETS_COVERS, AlbumID),
    [{AlbumID, {path, AlbumPathBin}}] = ets:lookup(?ETS_PATHS, AlbumID),
    [[{AlbumTuple, DateTuple}]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
    [{AlbumID, AlbumArtist}] = ets:lookup(?ETS_ARTISTS, AlbumID),
    FileBin = unicode:characters_to_binary(File),
    UrlCover = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, AlbumCover/binary>>,
    UrlFile  = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, FileBin/binary>>,
    Res = [AlbumID, {file, UrlFile}, {cover, UrlCover}, AlbumArtist, AlbumTuple, DateTuple, Title, TrackID],
    {ok, Res}.
