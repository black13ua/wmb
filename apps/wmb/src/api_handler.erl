%% Feel free to use, reuse and abuse the code in this file.

%% @doc API handler
-module(api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ets_names.hrl").

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    [_, _, APIType, APIid] = binary:split(Path, [<<"/">>], [global]),
    io:format("Path Elements: ~p~n", [[APIType, APIid]]),
    FilesUrlRoot = <<"/files/">>,

    case APIType of
        <<"albums">> ->
            AlbumID = {album_id, binary_to_integer(APIid)},
            [[{AlbumTuple, DateTuple}]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
            [{AlbumID, AlbumArtist}] = ets:lookup(?ETS_ARTISTS, AlbumID),
            [{AlbumID, {path, AlbumPathBin}}] = ets:lookup(?ETS_PATHS, AlbumID),
            [{AlbumID, {cover, AlbumCover}}] = ets:lookup(?ETS_COVERS, AlbumID),
            UrlCover = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, AlbumCover/binary>>,
            TracksList = ets:match(?ETS_TRACKS, {AlbumID, {'$2', '$1', '$3'}}),
            TracksListWithPath = lists:map(fun(X) ->
                                     TrackID = proplists:get_value(track_id, X),
                                     File = proplists:get_value(file, X),
                                     Title = proplists:get_value(title, X),
				     FullPath = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, File/binary>>,
				     [{file, FullPath}, {title, Title}, {track_id, TrackID}]  end, TracksList),
            io:format("Response from /api/albums/id: ~p~n", [[AlbumID, AlbumArtist, AlbumTuple, DateTuple, {cover, UrlCover}, {tracks, TracksListWithPath}]]),
            Res = jsx:encode([AlbumID, AlbumArtist, AlbumTuple, DateTuple, {cover, UrlCover}, {tracks, TracksListWithPath}]);
        <<"tracks">> ->
            [[AlbumID, {file, File}, Title]] = ets:match(?ETS_TRACKS, {'$1', {'$2', '$3', {track_id, binary_to_integer(APIid)}}}),
            [{AlbumID, {cover, AlbumCover}}] = ets:lookup(?ETS_COVERS, AlbumID),
            [{AlbumID, {path, AlbumPathBin}}] = ets:lookup(?ETS_PATHS, AlbumID),
            [[{AlbumTuple, DateTuple}]] = ets:match(?ETS_ALBUMS, {'$1', AlbumID}),
            [{AlbumID, AlbumArtist}] = ets:lookup(?ETS_ARTISTS, AlbumID),
            FileBin = unicode:characters_to_binary(File),
            io:format("Response from /api/tracks/id: ~p~n", [[AlbumID, AlbumArtist, Title, AlbumPathBin, AlbumCover]]),
            UrlCover = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, AlbumCover/binary>>,
            UrlFile  = <<FilesUrlRoot/binary, AlbumPathBin/binary, <<"/">>/binary, FileBin/binary>>,
            Res = jsx:encode([AlbumID, {file, UrlFile}, {cover, UrlCover}, AlbumArtist, AlbumTuple, DateTuple, Title]);
        _ ->
            Res = <<"Not Found">>
    end,

    {ok, Req2} = cowboy_req:reply(
               200,
               [{<<"content-type">>, <<"application/json">>}],
                   Res,
               Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
