-module(data_merger).
-export([
    get_all_dates/0,
    get_all_genres/0,
    get_all_letters/0,
    get_album_by_albumid/1,
    get_albums_by_albumidlist/2,
    get_albums/1, get_albums/3,
    get_albums_by_artistid/1,
    get_albums_by_genre_name/1, get_albums_by_genreid/1,
    get_albums_by_date/1, get_albums_by_date_tuple/1,
    get_albums_by_filter/2,
    get_albums_by_filters/1,
    get_albums_by_filters_v2/1,
    get_artist_by_albumid/1,
    get_artists_by_letterid/1,
    get_genre_by_genreid/1,
    get_path_by_pathid/1,
    get_random_tracks/1,
    get_track_by_trackid/1,
    get_tracklist_for_web/2,
    search_albums_by_phrase/1,
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
    FirstAlbumKey = wmb_helpers:skip_ets_elements(Skip, ?ETS_ALBUMS),
    io:format("Albums Format Selected: ~p~n", [[Format, Skip, Items]]),
    case Format of
        json ->
            io:format("JSON Format Selected: ~p~n", [[Format, Items]]); 
        tpl ->
            Result = get_tpl(FirstAlbumKey, Items, []),
            Result;
        _ -> 
            io:format("Unknown Albums Format Selected: ~p~n", [[Format, Items]])
    end.

-spec get_tpl({album_id, integer()}, integer(), list()) ->
    {ok, [proplists:proplist()]}.
get_tpl(AlbumKey, 1, ResultAcc) ->
    {ok, ResultFromEts} = get_album_by_albumid(AlbumKey),
    {ok, lists:reverse([ResultFromEts | ResultAcc])};
get_tpl(AlbumKey, Items, ResultAcc) ->
    {ok, ResultFromEts} = get_album_by_albumid(AlbumKey),
    case ets:next(?ETS_ALBUMS, AlbumKey) of
        '$end_of_table' ->
            {ok, lists:reverse([ResultFromEts | ResultAcc])};
        AlbumKeyNext ->
            get_tpl(AlbumKeyNext, Items - 1, [ResultFromEts | ResultAcc])
    end.

-spec get_albums_by_albumidlist([proplists:proplist()], list()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_albumidlist([[AlbumID]|Rest], Acc) ->
    {ok, Album} = get_album_by_albumid(AlbumID),
    get_albums_by_albumidlist(Rest, [Album|Acc]);
get_albums_by_albumidlist([], Acc) ->
    {ok, Acc}.

-spec get_album_by_albumid({album_id, integer()}) ->
    {ok, [proplists:proplist()]}.
get_album_by_albumid(AlbumKey) ->
    [{AlbumKey, AlbumValue}|_] = ets:lookup(?ETS_ALBUMS, AlbumKey),
    io:format("AlbumValue: ~p~n", [AlbumValue]),
    get_album_by_albumrow(AlbumKey, AlbumValue).

-spec get_album_by_albumrow({album_id, integer()}, {{album, bitstring()}, {date, bitstring()},
                            {tracks, list()}, {path_id, integer()}, {genre_id, bitstring()}, {cover_id, bitstring()}}) ->
    {ok, [proplists:proplist()]}.
get_album_by_albumrow(AlbumID, {AlbumTuple, DateTuple, AlbumTrackIDList, PathID, GenreID, CoverID}) ->
    {ok, AlbumArtist} = get_artist_by_albumid(AlbumID),
    {ok, Cover} = join_path_and_cover(PathID, CoverID),
    {ok, GenreTuple} = get_genre_by_genreid(GenreID),
    {ok, TracksList} = get_tracklist_for_web(AlbumID, AlbumTrackIDList),
    AlbumResult = [AlbumID, AlbumArtist, Cover, GenreTuple, {tracks, TracksList}, AlbumTuple, DateTuple],
    {ok, AlbumResult}.

-spec join_path_and_cover({path_id, integer()}, {cover_id, integer()}) ->
    {ok, {cover, bitstring()}}.
join_path_and_cover(PathID, CoverID) ->
    {ok, {path, Path}} = get_path_by_pathid(PathID),
    [{_, {cover, Cover}}] = ets:lookup(?ETS_COVERS, CoverID),
    UrlCover = <<?PATH_STATIC_WEB/binary, Path/binary, <<"/">>/binary, Cover/binary>>,
    io:format("Cover is: ~p~n", [UrlCover]),
    {ok, {cover, UrlCover}}.

-spec get_tracklist_for_web({album_id, integer()}, {tracks, list()}) ->
    {ok, []} | {ok, [proplists:proplist()]} | {error, atom()}.
get_tracklist_for_web(_AlbumID, {tracks, TracksList}) ->
    case TracksList of
        [] ->
            {ok, []};
        _ ->
            Fun = fun(X) ->
                      [{_, {_, {file, File}, {title, Title}, {path_id, PathID}}}] = ets:lookup(?ETS_TRACKS, {track_id, X}),
                      {ok, {path, Path}} = get_path_by_pathid({path_id, PathID}),
                      FullPath = <<?PATH_STATIC_WEB/binary, Path/binary, <<"/">>/binary, File/binary>>,
                      [{file, FullPath}, {title, Title}, {track_id, X}]
                  end,
            TracksListWithPath = lists:map(Fun, TracksList),
            {ok, TracksListWithPath}
    end.

%% Get AlbumList by ArtistID
-spec get_albums_by_artistid({artist_id, integer()}) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_artistid(ArtistID) ->
    AlbumIDList = ets:match(?ETS_ARTISTS, {'$1', {'_', ArtistID}}),
    get_albums_by_albumidlist(AlbumIDList, []).

%%% Get AlbumList by Genre Name
-spec get_albums_by_genre_name(bitstring()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_genre_name(GenreName) ->
    [[GenreID]] = ets:match(?ETS_GENRES, {'$1', {genre, GenreName}}),
    get_albums_by_genreid(GenreID).

%%% Get AlbumList by GenreID
-spec get_albums_by_genreid({genre_id, bitstring()}) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_genreid(GenreID) ->
    AlbumIDList = ets:match(?ETS_ALBUMS, {'$1', {'_', '_', '_', '_', GenreID, '_'}}),
    get_albums_by_albumidlist(AlbumIDList, []).

%%% Get AlbumList by Date Bin
-spec get_albums_by_date(bitstring()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_date(Date) ->
    get_albums_by_date_tuple({date, Date}).

%%% Get AlbumList by Date Tuple
-spec get_albums_by_date_tuple({date, bitstring()}) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_date_tuple(DateTuple) ->
    AlbumIDList = ets:match(?ETS_ALBUMS, {'$1', {'_', DateTuple, '_', '_', '_', '_'}}),
    get_albums_by_albumidlist(AlbumIDList, []).

%%% Get TrackList with N Random Tracks
-spec get_random_tracks(integer()) ->
    {ok, [proplists:proplist()]}.
get_random_tracks(N) ->
    TracksCount = ets:info(?ETS_TRACKS, size),
    %{ok, TracksListRandom} = get_random_tracks(N, crypto:rand_uniform(1, TracksCount), TracksCount, []),
    {ok, TracksListRandom} = get_random_tracks(N, rand:uniform(TracksCount), TracksCount, []),
    {ok, TracksListRandom}.

-spec get_random_tracks(integer(), integer(), integer(), list()) ->
    {ok, [proplists:proplist()]}.
get_random_tracks(0, _RandomID, _MaxID, Acc) ->
    {ok, Acc};
get_random_tracks(N, RandomID, MaxID, Acc) ->
    {ok, TrackJson} = get_track_by_trackid({track_id, RandomID}),
    io:format("RandomID: ~p~n", [[RandomID, ?MODULE]]),
    %get_random_tracks(N - 1, crypto:rand_uniform(1, MaxID), MaxID, [TrackJson|Acc]).
    get_random_tracks(N - 1, rand:uniform(MaxID), MaxID, [TrackJson|Acc]).

%%% Get Track by TrackID
-spec get_track_by_trackid({track_id, integer()}) ->
    {ok, [proplists:proplist()]}.
get_track_by_trackid(TrackID) ->
    [{_, {AlbumID, {file, File}, Title, PathID}}] = ets:lookup(?ETS_TRACKS, TrackID),
    [{_, {AlbumTuple, DateTuple, _AlbumTrackIDList, _, GenreID, CoverID}}] = ets:lookup(?ETS_ALBUMS, AlbumID),
    {ok, {path, PathBin}} = get_path_by_pathid(PathID),
    {ok, GenreTuple} = get_genre_by_genreid(GenreID),
    {ok, AlbumArtist} = get_artist_by_albumid(AlbumID),
    {ok, Cover} = join_path_and_cover(PathID, CoverID),
    UrlFile  = <<?PATH_STATIC_WEB/binary, PathBin/binary, <<"/">>/binary, File/binary>>,
    Res = [AlbumID, {file, UrlFile}, Cover, AlbumArtist, AlbumTuple, DateTuple, GenreTuple, Title, TrackID],
    {ok, Res}.

%%% Get Genre by GenreID
-spec get_genre_by_genreid({genre_id, integer()}) ->
    {ok, {genre, bitstring()}}.
get_genre_by_genreid(GenreID) ->
    [{GenreID, GenreTuple}] = ets:lookup(?ETS_GENRES, GenreID),
    {ok, GenreTuple}.

%%% Get Path by PathID
-spec get_path_by_pathid({path_id, integer()}) ->
    {ok, {path, bitstring()}}.
get_path_by_pathid(PathID) ->
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
    NextAlbumKey = wmb_helpers:skip_ets_elements(EtsSkip, ?ETS_ALBUMS),
    case NextAlbumKey of
        {error, _} ->
            {ok, Acc};
        {album_id, ID} ->
            [{_, {{album, AlbumName}, _, _, _, _, _}}] = ets:lookup(?ETS_ALBUMS, {album_id, ID}),
            MatchResult = re:run(AlbumName, Q, [caseless, unicode]),
            case MatchResult of
                {match, _} ->
                    {ok, Album} = get_album_by_albumid({album_id, ID}),
                    search_albums_by_phrase(N+1, EtsSkip+1, Q, [Album|Acc]);
                nomatch ->
                    search_albums_by_phrase(N, EtsSkip+1, Q, Acc)
            end
    end.

%%% Get Artist by AlbumID
-spec get_artist_by_albumid({album_id, integer()}) ->
    {ok, {artist, bitstring()}}.
get_artist_by_albumid(AlbumID) ->
    [{_, {AlbumArtist, _}}] = ets:lookup(?ETS_ARTISTS, AlbumID),
    {ok, AlbumArtist}.

%%% Get Artists by Letter ID
-spec get_artists_by_letterid({letter_id, integer()}) ->
    {ok, []} | {ok, list()}.
get_artists_by_letterid(LetterID) ->
    ArtistsFlat = ets:match(?ETS_ABC, {{LetterID, '_'}, '$1'}),
    ArtistsSorted = lists:flatten(lists:usort(ArtistsFlat)),
    Fun = fun(X) ->
              %[[ArtistID]|_] = ets:match(?ETS_ARTISTS, {'_', {X, '$1'}}),
              {[[ArtistID]], _} = ets:match(?ETS_ARTISTS, {'_', {X, '$1'}}, 1),
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
            {ok, {artist, AlbumArtistBin}} = get_artist_by_albumid(AlbumID),
            MatchResult = re:run(AlbumArtistBin, Q, [caseless, unicode]),
            case MatchResult of
                {match, _} ->
                    {ok, Album} = get_album_by_albumid(AlbumID),
                    search_artists_by_phrase(N+1, EtsSkip+1, Q, [Album|Acc]);
                nomatch ->
                    search_artists_by_phrase(N, EtsSkip+1, Q, Acc)
            end
    end.

%%% Get ABC Artists List for API /api/abc/abc
-spec get_all_letters() ->
    {ok, []} | {ok, list()}.
get_all_letters() ->
    LettersFlat = ets:match(?ETS_ABC, {{'$1', '$2'}, '_'}),
    LettersSorted = lists:usort(LettersFlat),
    {ok, LettersSorted}.

%%% Get Genres List for API
-spec get_all_genres() ->
    {ok, []} | {ok, list()}.
get_all_genres() ->
    Genres = ets:match(?ETS_GENRES, {'$1', '$2'}),
    {ok, Genres}.

%%% Get Dates List for API
-spec get_all_dates() ->
    {ok, []} | {ok, list()}.
get_all_dates() ->
    Dates = lists:usort(lists:flatten(ets:match(?ETS_ALBUMS, {'_', {'_', {date, '$1'}, '_', '_', '_', '_'}}))),
    {ok, Dates}.

%%% Get Albums by Filters
-spec get_albums_by_filters(Filters :: proplists:proplist()) ->
    {ok, []} | {ok, [proplists:proplist()]}.
get_albums_by_filters(Filters) ->
    io:format("We Here! ~p~n", [Filters]),
    Dates = proplists:get_value(<<"dates">>, Filters), 
    Genres = proplists:get_value(<<"genres">>, Filters), 
    {ok, FiltersMS} = generate_ms_for_filters(Dates, Genres),
    List = ets:select(?ETS_ALBUMS, FiltersMS),
    io:format("list! ~p~n", [List]),
    {ok, AlbumIDList} = get_albums_by_filter(Dates, Genres),
    io:format("IDlist! ~p~n", [AlbumIDList]),
    get_albums_by_albumidlist(AlbumIDList, []).

get_albums_by_filters_v2(Filters) ->
    io:format("We Here! ~p~n", [Filters]),
    Dates = proplists:get_value(<<"dates">>, Filters), 
    Genres = proplists:get_value(<<"genres">>, Filters), 
    get_albums_by_filters_v2(Dates, Genres).

-spec get_albums_by_filters_v2(Dates::list(), Genres::list()) ->
    {ok, [proplists:proplist()]}.
get_albums_by_filters_v2(Dates, Genres) ->
    {ok, FiltersMS} = generate_ms_for_filters(Dates, Genres),
    List = ets:select(?ETS_ALBUMS, FiltersMS),
    io:format("list! ~p~n", [List]),
    get_albums_by_albumidlist([[{album_id, AID}] || AID <- List], []).

-spec get_albums_by_filter(DatesList::list(), GenresList::list()) ->
    {ok, [proplists:proplist()]}.
get_albums_by_filter(DatesList, GenresList) ->
    Fun = fun({{album_id, A}, {_, {date, D}, _, _, {genre_id, G}, _}}, Acc) ->
              %io:format("X is: ~p~n", [[A, D, G]]),
              case lists:member(D, DatesList) of
                  true ->
                      case lists:member(G, GenresList) of
                          true ->
                              [[{album_id, A}]|Acc];
                          false ->
                              Acc
                      end;
                  false ->
                      Acc
              end
          end,
    Y = ets:foldl(Fun, [], ?ETS_ALBUMS),
    %io:format("Y is: ~p~n", [Y]),
    {ok, Y}.

-spec generate_ms_for_filters(LD :: list(), LG :: list()) ->
    {ok, proplists:proplist()}.
generate_ms_for_filters([], LG) ->
    LG_Expr = [{'==','$3', G} || G <- LG],
    TG_Expr = list_to_tuple(['orelse'|LG_Expr]),
    {ok, [{{{album_id,'$1'}, {'_',{date,'$2'},'_','_',{genre_id,'$3'},'_'}}, [TG_Expr], ['$1']}]};
generate_ms_for_filters(LD, []) ->
    LD_Expr = [{'==','$2', D} || D <- LD],
    TD_Expr = list_to_tuple(['orelse'|LD_Expr]),
    {ok, [{{{album_id,'$1'}, {'_',{date,'$2'},'_','_',{genre_id,'$3'},'_'}}, [TD_Expr], ['$1']}]};
generate_ms_for_filters(LD, LG) ->
    LG_Expr = [{'==','$3', G} || G <- LG],
    LD_Expr = [{'==','$2', D} || D <- LD],
    TG_Expr = list_to_tuple(['orelse'|LG_Expr]),
    TD_Expr = list_to_tuple(['orelse'|LD_Expr]),
    {ok, [{{{album_id,'$1'}, {'_',{date,'$2'},'_','_',{genre_id,'$3'},'_'}}, [TD_Expr, TG_Expr], ['$1']}]}.

