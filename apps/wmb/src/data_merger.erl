-module(data_merger).
-export([
    del_tracks_by_statemap/1,
    del_track_by_trackid/1,
    get_abc_letters/0,
    get_albums/1, get_albums/3,
    get_tracklist_by_albumid/1,
    get_track_by_trackid/1,
    get_album_by_albumid/1,
    get_album_by_albumtuple/1,
    get_albumlist_by_artistid/1,
    get_albums_by_genre_name/1, get_albums_by_genre_tuple/1,
    get_albums_by_date/1, get_albums_by_date_tuple/1,
    get_albumtuple_by_albumid/1,
    get_all_dates/0,
    get_all_genres/0,
    get_cover_by_albumid/1,
    get_path_by_albumid/1,
    get_path_by_pathid/1,
    get_random_tracks/1,
    search_albums_by_phrase/1,
    get_artists_by_letterid/1,
    search_artists_by_phrase/1, search_artists_by_phrase/4
]).

-include("ets_names.hrl").

-define(DEFAULT_ITEMS, 10).
-define(DEFAULT_SKIP,  1).
-define(PATH_STATIC_WEB, <<"/files/">>).


-spec get_albums(atom()) ->
    {ok, [proplists:proplist()]}.
get_albums(Format) ->
    get_albums(Format, ?DEFAULT_SKIP, ?DEFAULT_ITEMS).

% Format can be json or tpl
-spec get_albums(atom(), integer(), integer()) ->
    {ok, [proplists:proplist()]}.
get_albums(Format, Skip, Items) ->
    FirstAlbumTuple = wmb_helpers:skip_ets_elements(Skip, ?ETS_ALBUMS),
    io:format("Albums Format Selected: ~p~n", [[Format, Skip, Items]]),
    case Format of
        json ->
            io:format("JSON Format Selected: ~p~n", [[Format, Items]]); 
        tpl ->
            Result = get_tpl(FirstAlbumTuple, Items, []),
            Result;
        _ -> 
            io:format("Unknown Albums Format Selected: ~p~n", [[Format, Items]])
    end.

-spec get_tpl({{album, bitstring()}, {date, bitstring()}}, integer(), list()) ->
    {ok, [proplists:proplist()]}.
get_tpl(AlbumTuple, 1, ResultAcc) ->
    {ok, ResultFromEts} = get_album_by_albumtuple(AlbumTuple),
    {ok, lists:reverse([ResultFromEts | ResultAcc])};
get_tpl(AlbumTuple, Items, ResultAcc) ->
    {ok, ResultFromEts} = get_album_by_albumtuple(AlbumTuple),
    case ets:next(?ETS_ALBUMS, AlbumTuple) of
        '$end_of_table' ->
            {ok, lists:reverse([ResultFromEts | ResultAcc])};
        AlbumTupleNext ->
            get_tpl(AlbumTupleNext, Items - 1, [ResultFromEts | ResultAcc])
    end.

-spec get_album_by_albumtuple({{album, bitstring()}, {date, bitstring()}}) ->
    {ok, [proplists:proplist()]}.
get_album_by_albumtuple(AlbumTuple) ->
    [{AlbumTuple, AlbumID}|_] = ets:lookup(?ETS_ALBUMS, AlbumTuple),
    get_album_by_albumid(AlbumID).

-spec get_album_by_albumid({album_id, integer()}) ->
    {ok, [proplists:proplist()]}.
get_album_by_albumid(AlbumID) ->
    {ok, AlbumTuple} = get_albumtuple_by_albumid(AlbumID),
    [{AlbumTuple, AlbumID}|_] = ets:lookup(?ETS_ALBUMS, AlbumTuple),
    [{AlbumID, AlbumArtist, _}] = ets:lookup(?ETS_ARTISTS, AlbumID),
    [{AlbumID, GenreTuple}] = ets:lookup(?ETS_GENRES, AlbumID),
    {ok, PathTuple} = get_path_by_albumid(AlbumID),
    io:format("AlbumID is: ~p~n", [AlbumID]),
    {ok, Cover} = get_cover_by_albumid(AlbumID),
    {ok, TracksList} = get_tracklist_by_albumid(AlbumID),
    AlbumList = erlang:tuple_to_list(AlbumTuple),
    AlbumResult = [AlbumID, AlbumArtist, {cover, Cover}, GenreTuple, PathTuple, {tracks, TracksList} | AlbumList],
    {ok, AlbumResult}.

-spec get_tracklist_by_albumid({album_id, integer()}) ->
    {ok, []} | {ok, [proplists:proplist()]} | {error, atom()}.
get_tracklist_by_albumid(AlbumID) ->
    {ok, {path, AlbumPathBin}} = get_path_by_albumid(AlbumID),
    TracksList = ets:match(?ETS_TRACKS, {AlbumID, {'$1', '$2', '$3', '_'}}),
    case TracksList of
        [] ->
            {error, trackslist_empty};
        _ ->
            Fun = fun(X) ->
                      File = proplists:get_value(file, X),
                      Title = proplists:get_value(title, X),
                      TrackID = proplists:get_value(track_id, X),
                      FullPath = <<?PATH_STATIC_WEB/binary, AlbumPathBin/binary, <<"/">>/binary, File/binary>>,
                      [{file, FullPath}, {title, Title}, {track_id, TrackID}]
                  end,
            TracksListWithPath = lists:map(Fun, TracksList),
            {ok, TracksListWithPath}
    end.

%%% Get AlbumTuple by AlbumID
-spec get_albumtuple_by_albumid({album_id, integer()}) ->
    {ok, {{album, bitstring()}, {date, bitstring()}}}.
get_albumtuple_by_albumid(AlbumID) ->
    [[AlbumTuple]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
    io:format("AlbumTuple is: ~p~n", [AlbumTuple]),
    {ok, AlbumTuple}.

%%% Get AlbumList by ArtistID
-spec get_albumlist_by_artistid(integer()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albumlist_by_artistid(ArtistID) ->
    AlbumIDList = lists:flatten(ets:match(?ETS_ARTISTS, {'$1', '_', {artist_id, ArtistID}})),
    get_albumlist_by_artistid(AlbumIDList, []).

-spec get_albumlist_by_artistid(list(), list()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albumlist_by_artistid([], Acc) ->
    {ok, Acc};
get_albumlist_by_artistid([AlbumID|AlbumIDList], Acc) ->
    {ok, {ArtistTuple, DateTuple}} = get_albumtuple_by_albumid(AlbumID),
    get_albumlist_by_artistid(AlbumIDList, [[ArtistTuple, DateTuple, AlbumID]|Acc]).

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
    {ok, R} = get_album_by_albumtuple(AlbumTuple),
    get_albums_by_albumtuplelist(Rest, [R|Acc]);
get_albums_by_albumtuplelist([], Acc) ->
    {ok, Acc}.

%%% Get AlbumList by Date
-spec get_albums_by_date(bitstring()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_date(Date) ->
    get_albums_by_date_tuple({date, Date}).

%%% Get AlbumList by Date Tuple
-spec get_albums_by_date_tuple({date, bitstring()}) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_date_tuple(DateTuple) ->
    AlbumIDList = ets:match(?ETS_ALBUMS, {{'_', DateTuple}, '$1'}),
    get_albums_by_albumtuplelist(AlbumIDList, []).

%%% Get TrackList with N Random Tracks
-spec get_random_tracks(integer()) ->
    {ok, [proplists:proplist()]}.
get_random_tracks(N) ->
    TracksCount = ets:info(?ETS_TRACKS, size),
    {ok, TracksListRandom} = get_random_tracks(N, crypto:rand_uniform(1, TracksCount), TracksCount, []),
    {ok, TracksListRandom}.

-spec get_random_tracks(integer(), integer(), integer(), list()) ->
    {ok, [proplists:proplist()]}.
get_random_tracks(0, _RandomID, _MaxID, Acc) ->
    {ok, Acc};
get_random_tracks(N, RandomID, MaxID, Acc) ->
    {ok, TrackJson} = get_track_by_trackid({track_id, RandomID}),
    io:format("RandomID: ~p~n", [[RandomID, ?MODULE]]),
    get_random_tracks(N - 1, crypto:rand_uniform(1, MaxID), MaxID, [TrackJson|Acc]).

%%% Get Track by TrackID
-spec get_track_by_trackid({track_id, integer()}) ->
    {ok, [proplists:proplist()]}.
get_track_by_trackid(TrackID) ->
    [[AlbumID, {file, File}, Title, PathID]] = ets:match(?ETS_TRACKS, {'$1', {'$2', '$3', TrackID, '$4'}}),
    {ok, {path, AlbumPathBin}} = get_path_by_pathid(PathID),
    [[{AlbumTuple, DateTuple}]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
    [{AlbumID, AlbumArtist, _}] = ets:lookup(?ETS_ARTISTS, AlbumID),
    {ok, UrlCover} = get_cover_by_albumid(AlbumID),
    UrlFile  = <<?PATH_STATIC_WEB/binary, AlbumPathBin/binary, <<"/">>/binary, File/binary>>,
    Res = [AlbumID, {file, UrlFile}, {cover, UrlCover}, AlbumArtist, AlbumTuple, DateTuple, Title, TrackID],
    {ok, Res}.

%%% Get Cover by AlbumID
-spec get_cover_by_albumid({album_id, integer()}) ->
    {ok, bitstring()}.
get_cover_by_albumid(AlbumID) ->
    [{AlbumID, {cover, AlbumCover}}] = ets:lookup(?ETS_COVERS, AlbumID),
    {ok, {path, AlbumPathBin}} = get_path_by_albumid(AlbumID),
    UrlCover = <<?PATH_STATIC_WEB/binary, AlbumPathBin/binary, <<"/">>/binary, AlbumCover/binary>>,
    {ok, UrlCover}.

%%% Get Path by AlbumID
-spec get_path_by_albumid({album_id, integer()}) ->
    {ok, {path, bitstring()}}.
get_path_by_albumid(AlbumID) ->
    io:format("AlbumID is: ~p~n", [AlbumID]),
    [[Path]] = ets:match(?ETS_PATHS, {'_', {'$1', AlbumID}}),
    {ok, Path}.

%%% Get Path by PathID
-spec get_path_by_pathid({path_id, integer()}) ->
    {ok, {path, bitstring()}}.
get_path_by_pathid(PathID) ->
    io:format("PathID is: ~p~n", [PathID]),
    [{PathID, {{path, AlbumPathBin}, _}}] = ets:lookup(?ETS_PATHS, PathID),
    {ok, {path, AlbumPathBin}}.

%%% Search Albums by Phrase
-spec search_albums_by_phrase(bitstring()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
search_albums_by_phrase(Q) ->
    search_albums_by_phrase(0, 0, Q, []).

-spec search_albums_by_phrase(integer(), integer(), bitstring(), list()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
search_albums_by_phrase(?DEFAULT_ITEMS, _EtsSkip, _Q, Acc) ->
    {ok, Acc};
search_albums_by_phrase(N, EtsSkip, Q, Acc) ->
    NextAlbumTuple = wmb_helpers:skip_ets_elements(EtsSkip, ?ETS_ALBUMS),
    case NextAlbumTuple of
        {error, _} ->
            {ok, Acc};
        {{album, FirstAlbumTuple}, {date, Date}} ->
            MatchResult = re:run(FirstAlbumTuple, Q, [caseless, unicode]),
            case MatchResult of
                {match, _} ->
                    {ok, Album} = get_album_by_albumtuple({{album, FirstAlbumTuple}, {date, Date}}),
                    search_albums_by_phrase(N+1, EtsSkip+1, Q, [Album|Acc]);
                nomatch ->
                    search_albums_by_phrase(N, EtsSkip+1, Q, Acc)
            end
    end.

%%% Get Artists by Letter ID
-spec get_artists_by_letterid({letter_id, integer()}) ->
    {ok, []} | {ok, list()}.
get_artists_by_letterid(LetterID) ->
    ArtistsFlat = ets:match(?ETS_ABC, {{LetterID, '_'}, '$1'}),
    ArtistsSorted = lists:flatten(lists:usort(ArtistsFlat)),
    Fun = fun(X) ->
              [[ArtistID]|_] = ets:match(?ETS_ARTISTS, {'_', X, '$1'}),
              [ArtistID, X]
          end,
    Artists = lists:map(Fun, ArtistsSorted),
    {ok, Artists}.

%%% Search Artists in ETS
-spec search_artists_by_phrase(bitstring()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
search_artists_by_phrase(Q) ->
    search_artists_by_phrase(0, 0, Q, []).

-spec search_artists_by_phrase(integer(), integer(), bitstring(), list()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
search_artists_by_phrase(?DEFAULT_ITEMS, _EtsSkip, _Q, Acc) ->
    {ok, Acc};
search_artists_by_phrase(N, EtsSkip, Q, Acc) ->
    AlbumID = wmb_helpers:skip_ets_elements(EtsSkip, ?ETS_ARTISTS),
    case AlbumID of
        {error, _} ->
            {ok, Acc};
        {album_id, _ID} ->
            [{AlbumID, {artist, AlbumArtist}, _}] = ets:lookup(?ETS_ARTISTS, AlbumID),
            MatchResult = re:run(AlbumArtist, Q, [caseless, unicode]),
            case MatchResult of
                {match, _} ->
                    {ok, Album} = get_album_by_albumid(AlbumID),
                    search_artists_by_phrase(N+1, EtsSkip+1, Q, [Album|Acc]);
                nomatch ->
                    search_artists_by_phrase(N, EtsSkip+1, Q, Acc)
            end
    end.

%%% Get ABC Artists List for API /api/abc/abc
-spec get_abc_letters() ->
    {ok, []} | {ok, list()}.
get_abc_letters() ->
    LettersFlat = ets:match(?ETS_ABC, {{'$1', '$2'}, '_'}),
    LettersSorted = lists:usort(LettersFlat),
    {ok, LettersSorted}.

%%% Get Genres List for API
-spec get_all_genres() ->
    {ok, []} | {ok, list()}.
get_all_genres() ->
    Genres = lists:usort(lists:flatten(ets:match(?ETS_GENRES, {'_', {genre,'$2'}}))),
    {ok, Genres}.

%%% Get Dates List for API
-spec get_all_dates() ->
    {ok, []} | {ok, list()}.
get_all_dates() ->
    Dates = lists:usort(lists:flatten(ets:match(?ETS_ALBUMS, {{{album, '_'}, {date, '$1'}}, {album_id, '_'}}))),
    {ok, Dates}.

%%% DEL Tracks from ETS by tracksMap from State wmb_librarian (if Dir moved or removed)
-spec del_tracks_by_statemap(map()) ->
    {ok, empty} | {ok, cleaned}.
del_tracks_by_statemap(Map) when map_size(Map) == 0 ->
    {ok, empty};
del_tracks_by_statemap(Map) ->
    Files = maps:keys(Map),
    Fun = fun(File, Acc) ->
              FileMap = maps:get(File, Map),
              TrackID = maps:get(track_id, FileMap),
              {ok, AlbumID} = del_track_by_trackid({track_id, TrackID}),
              [AlbumID|Acc]
          end,
    AlbumIDList = lists:usort(lists:foldl(Fun, [], Files)),
    io:format("AlbumIDList: ~p~n", [AlbumIDList]),
    {ok, cleaned}.

del_track_by_trackid(TrackID) ->
    [[AlbumID, {file, _File}, _Title]] = ets:match(?ETS_TRACKS, {'$1', {'$2', '$3', TrackID, '_'}}),
    Deleted = ets:match_delete(wmb_tracks, {'$1', {'$2', '$3', TrackID, '$4'}}),
    {ok, UrlCover} = get_cover_by_albumid(AlbumID),
    {ok, PathTuple} = get_path_by_albumid(AlbumID),
    [[{_AlbumTuple, DateTuple}]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
    [{AlbumID, AlbumArtist, _}] = ets:lookup(?ETS_ARTISTS, AlbumID),
    io:format("del_track: ~p~n", [[TrackID, AlbumID, {cover, UrlCover}, PathTuple, DateTuple, AlbumArtist, Deleted]]),
    {ok, AlbumID}.

