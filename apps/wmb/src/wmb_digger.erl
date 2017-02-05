%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(wmb_digger).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Internal Functions
-export([parse_file/1]).

-include("ets_names.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({parse, File, FileID3Tags}, _From, State) ->
    Album  = maps:get(<<"ALBUM">>,  FileID3Tags, <<"Undef_Album">>),
    Artist = maps:get(<<"ARTIST">>, FileID3Tags, <<"Undef_Artist">>),
    AlbumArtist = maps:get(<<"ALBUMARTIST">>, FileID3Tags, <<"Undef_Artist">>),
    Genre  = maps:get(<<"GENRE">>,  FileID3Tags, <<"Undef_Genre">>),
    Date   = maps:get(<<"DATE">>,   FileID3Tags, <<"Undef_Date">>),
    Title  = maps:get(<<"TITLE">>,  FileID3Tags, <<"Undef_Title">>),
    FileBasename = unicode:characters_to_binary(filename:basename(File)),
    TrackID = erlang:unique_integer([positive, monotonic]),
    case get_album_id(Album, Date) of
        undefined ->
            {ok, FilesRoot} = application:get_env(wmb, files_root),
            FilePathFull = lists:concat([FilesRoot, "/", File]),
            AlbumPathRelBin = unicode:characters_to_binary(filename:dirname(File)),
            AlbumPathFull = filename:dirname(FilePathFull),
            io:format("Files and Tags: ~p~n", [{AlbumPathFull, FileBasename, FileID3Tags}]),
            AlbumID = erlang:unique_integer([positive, monotonic]),
            ets:insert(?ETS_ALBUMS,  {{{album, Album}, {date, Date}}, {album_id, AlbumID}}),
            ets:insert(?ETS_ARTISTS, {{album_id, AlbumID}, {artist, AlbumArtist}}),
            ets:insert(?ETS_TRACKS,  {{album_id, AlbumID}, {{file, FileBasename}, {title, Title}, {track_id, TrackID}}}),
            ets:insert(?ETS_GENRES,  {{album_id, AlbumID}, {genre, Genre}}),
            ets:insert(?ETS_PATHS,   {{album_id, AlbumID}, {path, AlbumPathRelBin}}),
            {ok, PossibleCoversList} = application:get_env(wmb, possible_covers_list),
            {ok, AlbumFilesList} = file:list_dir(AlbumPathFull),
            {_, AlbumCover} = find_album_cover(AlbumFilesList, PossibleCoversList),
            ets:insert(?ETS_COVERS, {{album_id, AlbumID}, {cover, AlbumCover}}),
            io:format("Album Cover is: ~p~n", [AlbumCover]);
        ExistedAlbumID ->
            ets:insert(?ETS_TRACKS, {{album_id, ExistedAlbumID}, {{file, FileBasename}, {title, Title}, {track_id, TrackID}}})
    end,
    {reply, FileID3Tags, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Exported functions
%%%===================================================================
parse_file(File) ->
    {ok, FilesRoot} = application:get_env(wmb, files_root),
    FilePathFull = lists:concat([FilesRoot, "/", File]),
    case flactags:get_tags(FilePathFull) of
        {ok, FileMetadata} ->
            FileID3Tags = maps:get(4, FileMetadata),
            gen_server:call(?SERVER, {parse, File, FileID3Tags});
        {error, Error} ->
            ets:insert(?ETS_ERRORS, {{file, File}, {error, Error}})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_album_id(Album, Date) ->
    case ets:lookup(?ETS_ALBUMS, {{album, Album}, {date, Date}}) of
        [] ->
            undefined;
        [{_, {album_id, AlbumID}}|_] ->
            AlbumID
    end.

find_album_cover(AlbumFilesList, [PossibleCover|RestPossibleCovers]) ->
    case lists:member(PossibleCover, AlbumFilesList) of
        true ->
            Cover = unicode:characters_to_binary(PossibleCover),
            {ok, Cover};
        false ->
            find_album_cover(AlbumFilesList, RestPossibleCovers)
    end;
find_album_cover(AlbumFilesList, []) ->
    {error, cover_not_found}.
