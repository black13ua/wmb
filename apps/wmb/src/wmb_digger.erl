-module(wmb_digger).

%% Exported Functions
-export([parse_file/1, parse_file/2]).

-include("ets_names.hrl").

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
            add_to_ets(FilePathFull, FileID3Tags);
            %gen_server:call(?SERVER, {parse, FilePathFull, FileID3Tags});
        {error, Error} ->
            ets:insert(?ETS_ERRORS, {{file, FilePathFull}, {error, Error}})
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
    {ok, FileRel} = wmb_helpers:get_rel_path(File),
    FileBasename = unicode:characters_to_binary(filename:basename(FileRel)),
    TrackID = ets:update_counter(?ETS_COUNTERS, track_id_counter, 1),
    case get_album_id(Album, Date) of
        undefined ->
            %{ok, FilesRoot} = application:get_env(wmb, files_root),
            %FilePathFull = lists:concat([FilesRoot, "/", File]),
            AlbumPathRelBin = unicode:characters_to_binary(filename:dirname(FileRel)),
            AlbumPathFull = filename:dirname(File),
            %%%io:format("Files and Tags: ~p~n", [{AlbumPathFull, FileBasename, FileID3Tags}]),
            ArtistID = get_artist_id(AlbumArtist),
            %%%io:format("ArtistID is: ~p~n", [ArtistID]),
            AlbumID = ets:update_counter(?ETS_COUNTERS, album_id_counter, 1),
            ets:insert(?ETS_ALBUMS,  {{{album, Album}, {date, Date}}, {album_id, AlbumID}}),
            % new
            % ets:insert(?ETS_ALBUMS,  {{{album, Album}, {date, Date}}, {{album_id, AlbumID}, {trackslist, [TrackID]}} }),
            ets:insert(?ETS_ARTISTS, {{album_id, AlbumID}, {artist, AlbumArtist}, {artist_id, ArtistID}}),
            ets:insert(?ETS_TRACKS,  {{album_id, AlbumID}, {{file, FileBasename}, {title, Title}, {track_id, TrackID}}}),
            ets:insert(?ETS_GENRES,  {{album_id, AlbumID}, {genre, Genre}}),
            ets:insert(?ETS_PATHS,   {{album_id, AlbumID}, {path, AlbumPathRelBin}}),
            %%
            %% FIXME Overhead!
            {ok, PossibleCoversList} = application:get_env(wmb, possible_covers_list),
            {ok, AlbumFilesList} = file:list_dir(AlbumPathFull),
            %% !FIXME
            %% 
            {_, AlbumCover} = find_album_cover(AlbumFilesList, PossibleCoversList),
            ets:insert(?ETS_COVERS, {{album_id, AlbumID}, {cover, AlbumCover}}),
            %%%io:format("Album Cover is: ~p~n", [AlbumCover]),
            [LetterByte|_] = unicode:characters_to_list(AlbumArtist),
            LetterBin = unicode:characters_to_binary([LetterByte]),
            LetterID = get_letter_id(LetterBin),
            ets:insert(?ETS_ABC, {{{letter_id, LetterID}, {letter, LetterBin}}, {artist, AlbumArtist}});
            %%%io:format("Letters is: ~p~n", [[LetterByte, LetterBin]]);
        ExistedAlbumID ->
            ets:insert(?ETS_TRACKS, {{album_id, ExistedAlbumID}, {{file, FileBasename}, {title, Title}, {track_id, TrackID}}})
    end,
    {ok, {track_id, TrackID}}.

-spec get_album_id(bitstring(), bitstring()) ->
    undefined | integer().
get_album_id(Album, Date) ->
    case ets:lookup(?ETS_ALBUMS, {{album, Album}, {date, Date}}) of
        [] ->
            undefined;
        [{_, {album_id, AlbumID}}|_] ->
        % new
        % [{_, {{album_id, AlbumID}, _}}|_] ->
            AlbumID
    end.

-spec get_artist_id(bitstring()) ->
    integer().
get_artist_id(AlbumArtist) ->
    case ets:match(?ETS_ARTISTS, {'_', {artist, AlbumArtist}, '$1'}) of
        [] ->
            ArtistID = ets:update_counter(?ETS_COUNTERS, artist_id_counter, 1),
            ArtistID;
        [[{artist_id, ArtistID}]|_] ->
            ArtistID
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

-spec find_album_cover(list(), list()) ->
    {ok, bitstring()} | {error, bitstring()}.
find_album_cover(AlbumFilesList, [PossibleCover|RestPossibleCovers]) ->
    case lists:member(PossibleCover, AlbumFilesList) of
        true ->
            Cover = unicode:characters_to_binary(PossibleCover),
            {ok, Cover};
        false ->
            find_album_cover(AlbumFilesList, RestPossibleCovers)
    end;
find_album_cover(_AlbumFilesList, []) ->
    {error, <<"cover_not_found">>}.

