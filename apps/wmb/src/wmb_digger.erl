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
handle_call({parse, File}, _From, State) ->
    AlbumPath = filename:dirname(File),
    FileMetadata = flactags:get_tags(File),
    FileMetadataBlock4 = maps:get(4, FileMetadata),
    io:format("~p~n: ", [{AlbumPath, File, FileMetadataBlock4}]),
    Album  = maps:get(<<"ALBUM">>,  FileMetadataBlock4, "Undef_Album"),
    Artist = maps:get(<<"ARTIST">>, FileMetadataBlock4, "Undef_Artist"),
    Genre  = maps:get(<<"GENRE">>,  FileMetadataBlock4, "Undef_Genre"),
    Date   = maps:get(<<"DATE">>,   FileMetadataBlock4, "Undef_Date"),
    Title  = maps:get(<<"TITLE">>,  FileMetadataBlock4, "Undef_Title"),
    case get_album_id(Artist, Album, Date, Genre) of
        undefined ->
            AlbumID = erlang:unique_integer([positive, monotonic]),
            ets:insert(wmb_albums, {{{artist, Artist}, {album, Album}, {date, Date}}, {album_id, AlbumID}}),
            ets:insert(wmb_tracks, {{album_id, AlbumID}, {{file, File}, {title, Title}}}),
            ets:insert(wmb_genres, {{album_id, AlbumID}, {genre, Genre}}),
            ets:insert(wmb_paths,  {{album_id, AlbumID}, {path, AlbumPath}});
        ExistedAlbumID ->
            ets:insert(wmb_tracks, {{album_id, ExistedAlbumID}, {{file, File}, {title, Title}}})
    end,
    
    {reply, FileMetadata, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

get_album_id(Artist, Album, Date, Genre) ->
    case ets:lookup(wmb_albums, {{artist, Artist}, {album, Album}, {date, Date}}) of
        [] ->
            undefined;
        [{_, {album_id, AlbumID}}|_] ->
            AlbumID
    end.



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
%%% Internal functions
%%%===================================================================
parse_file(File) ->
    gen_server:call(?SERVER, {parse, File}).

