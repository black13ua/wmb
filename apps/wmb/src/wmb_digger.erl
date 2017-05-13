-module(wmb_digger).

%% Gen Server
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {}).
%%

%% Exported Functions
-export([parse_file/1, parse_file/2]).
-export([find_album_cover/1, find_album_cover/2]).
-export([get_album_id/2, get_path_id/2]).

-include("ets_names.hrl").


start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
        {ok, #state{}}.

handle_call({File, FileID3Tags}, _From, State) ->
    Result = add_to_ets(File, FileID3Tags),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Exported functions
%%%===================================================================
-spec parse_file(string()) ->
    {ok, {track_id, integer()}} | false.
parse_file(File) ->
    {ok, FilesRoot} = application:get_env(wmb, files_root),
    FilePathFull = lists:concat([FilesRoot, '/', File]),
    parse_file(fullpath, FilePathFull).

-spec parse_file(fullpath, string()) ->
    {ok, {track_id, integer()}} | false.
parse_file(fullpath, FilePathFull) ->
    case flactags:get_tags(FilePathFull) of
        {ok, FileMetadata} ->
            FileID3Tags = maps:get(4, FileMetadata),
            ResultGenServer = gen_server:call(?SERVER, {FilePathFull, FileID3Tags}, 30000),
            ResultGenServer;
            %add_to_ets(FilePathFull, FileID3Tags);
        {error, Error} ->
            ets:insert_new(?ETS_ERRORS, {{file, FilePathFull}, {error, Error}})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_to_ets(string(), map()) ->
    {ok, {track_id, integer()}}.
add_to_ets(File, FileID3Tags) ->
    Album  = maps:get(<<"ALBUM">>,  FileID3Tags, <<"Undef_Album">>),
    %% Artist = maps:get(<<"ARTIST">>, FileID3Tags, <<"Undef_Artist">>),
    AlbumArtist = maps:get(<<"ALBUMARTIST">>, FileID3Tags, <<"Undef_Artist">>),
    Genre  = maps:get(<<"GENRE">>,  FileID3Tags, <<"Undef_Genre">>),
    Date   = maps:get(<<"DATE">>,   FileID3Tags, <<"Undef_Date">>),
    Title  = maps:get(<<"TITLE">>,  FileID3Tags, <<"Undef_Title">>),
    {ok, {AlbumPathRelBin, FileBasename}} = wmb_helpers:split_path_and_filename(File),
    TrackID = ets:update_counter(?ETS_COUNTERS, track_id_counter, 1),
    case get_album_id(Album, Date) of
        undefined ->
            AlbumID = ets:update_counter(?ETS_COUNTERS, album_id_counter, 1),
            AlbumPathFull = filename:dirname(File),
            ArtistID = get_artist_id(AlbumArtist),
            GenreID = get_genre_id(Genre),
            PathID = get_path_id(AlbumPathRelBin, AlbumID),
            {ok, AlbumCover} = find_album_cover(AlbumPathFull),
            CoverID = get_cover_id(AlbumCover),
            %io:format("Path & PathID is: ~p~n", [[AlbumPathRelBin, PathID]]),
            ets:insert_new(?ETS_ALBUMS, {{album_id, AlbumID}, {{album, Album}, {date, Date}, {tracks, [TrackID]}, {path_id, PathID}, {genre_id, GenreID}, {cover_id, CoverID}}}),
            ets:insert_new(?ETS_ARTISTS, {{album_id, AlbumID}, {{artist, AlbumArtist}, {artist_id, ArtistID}}}),
            ets:insert_new(?ETS_TRACKS, {{track_id, TrackID}, {{album_id, AlbumID}, {file, FileBasename}, {title, Title}, {path_id, PathID}}}),
            %%%io:format("Album Cover is: ~p~n", [AlbumCover]),
            [LetterByte|_] = unicode:characters_to_list(AlbumArtist),
            LetterBin = unicode:characters_to_binary([LetterByte]),
            LetterID = get_letter_id(LetterBin),
            ets:insert_new(?ETS_ABC, {{{letter_id, LetterID}, {letter, LetterBin}}, {artist, AlbumArtist}});
            %%%io:format("Letters is: ~p~n", [[LetterByte, LetterBin]]);
        ExistedAlbumID ->
            {{album, Album}, {date, Date}, {tracks, TrackIDList}, PathIDTuple, GenreTuple, CoverTuple} = ets:lookup_element(?ETS_ALBUMS, {album_id, ExistedAlbumID}, 2),
            ets:update_element(?ETS_ALBUMS, {album_id, ExistedAlbumID}, {2, {{album, Album}, {date, Date}, {tracks, [TrackID|TrackIDList]}, PathIDTuple, GenreTuple, CoverTuple}}),
            ets:insert_new(?ETS_TRACKS, {{track_id, TrackID}, {{album_id, ExistedAlbumID}, {file, FileBasename}, {title, Title}, PathIDTuple}})
    end,
    io:format("File Added: ~p~n", [[TrackID, File]]),
    {ok, {track_id, TrackID}}.

-spec get_album_id(bitstring(), bitstring()) ->
    undefined | integer().
get_album_id(Album, Date) ->
    case ets:match(?ETS_ALBUMS, {'$1', {{album, Album}, {date, Date}, '_', '_', '_', '_'}}) of
        [] ->
            undefined;
        [[{album_id, AlbumID}]] ->
            AlbumID;
        [[{album_id, AlbumID}]|_] ->
            io:format("ERROR, can't get AlbumID: ~p~n", [[Album, Date]]),
            AlbumID
    end.

-spec get_artist_id(bitstring()) ->
    integer().
get_artist_id(AlbumArtist) ->
    case ets:match(?ETS_ARTISTS, {'_', {{artist, AlbumArtist}, '$1'}}) of
        [] ->
            ArtistID = ets:update_counter(?ETS_COUNTERS, artist_id_counter, 1),
            ArtistID;
        [[{artist_id, ArtistID}]|_] ->
            ArtistID
    end.

-spec get_cover_id(bitstring()) ->
    integer().
get_cover_id(Cover) ->
    case ets:match(?ETS_COVERS, {'$1', {cover, Cover}}) of
        [] ->
            CoverID = ets:update_counter(?ETS_COUNTERS, cover_id_counter, 1),
            ets:insert_new(?ETS_COVERS, {{cover_id, CoverID}, {cover, Cover}}),
            CoverID;
        [[{cover_id, CoverID}]|_] ->
            CoverID
    end.

-spec get_genre_id(bitstring()) ->
    integer().
get_genre_id(Genre) ->
    case ets:match(?ETS_GENRES, {'$1', {genre, Genre}}) of
        [] ->
            GenreID = ets:update_counter(?ETS_COUNTERS, genre_id_counter, 1),
            ets:insert_new(?ETS_GENRES, {{genre_id, GenreID}, {genre, Genre}}),
            GenreID;
        [[{genre_id, GenreID}]|_] ->
            GenreID
    end.

-spec get_letter_id(bitstring()) ->
    integer().
get_letter_id(LetterBin) ->
    case ets:match(?ETS_ABC, {{'$1', {letter, LetterBin}}, '_'}) of
        [] ->
            ets:update_counter(?ETS_COUNTERS, letter_id_counter, 1);
        [[{letter_id, LetterID}]|_] ->
            LetterID
    end.

-spec get_path_id(bitstring(), integer()) ->
    integer().
get_path_id(AlbumPathRelBin, AlbumID) ->
    case ets:match(?ETS_PATHS, {'$1', {{path, AlbumPathRelBin}, {album_id, AlbumID}}}) of
        [] ->
            PathID = ets:update_counter(?ETS_COUNTERS, path_id_counter, 1),
            ets:insert_new(?ETS_PATHS, {{path_id, PathID}, {{path, AlbumPathRelBin}, {album_id, AlbumID}}}),
            PathID;
        [[{path_id, PathID}]] ->
            PathID
    end.

-spec find_album_cover(bitstring()) ->
    {ok, bitstring()}.
find_album_cover(AlbumPathFull) ->
    {ok, PossibleCoversList} = application:get_env(wmb, possible_covers_list),
    find_album_cover(AlbumPathFull, PossibleCoversList).

-spec find_album_cover(bitstring(), list()) ->
    {ok, bitstring()}.
find_album_cover(AlbumPathFull, [PossibleCover|RestPossibleCovers]) ->
    case filelib:is_file(lists:concat([AlbumPathFull, '/', PossibleCover])) of
        true ->
            Cover = unicode:characters_to_binary(PossibleCover),
            {ok, Cover};
        false ->
            find_album_cover(AlbumPathFull, RestPossibleCovers)
    end;
find_album_cover(_AlbumPathFull, []) ->
    {ok, <<"cover_not_found">>}.

