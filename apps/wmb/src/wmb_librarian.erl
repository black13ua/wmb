%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(wmb_librarian).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {path = undefined, timeout = undefined, files = #{}}).

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
start_link(Path) ->
    gen_server:start_link(?MODULE, [Path], []).

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
init([Path]) ->
    self() ! scan_directory,
    {ok, #state{path = Path}}.

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
handle_info(scan_directory, #state{path = Path} = State) ->
    {ok, NewState} = check_dir_or_file(Path, State),
    {ok, RescanTimeout} = application:get_env(wmb, rescan_timeout),
    DelayRandom = crypto:rand_uniform(1, 30000),
    Timeout = RescanTimeout * 1000 + DelayRandom,
    io:format("State Now: ~p~n: ", [[Timeout, NewState#state{timeout = Timeout}, self()]]),
    timer:send_after(Timeout, rescan_directory),
    {noreply, NewState#state{timeout = Timeout}};
handle_info(rescan_directory, #state{path = Path, timeout = Timeout} = State) ->
    DateNow = calendar:local_time(),
    io:format("Rescan Path now: ~p~n: ", [[Timeout, DateNow, Path]]),
    timer:send_after(Timeout, rescan_directory),
    {noreply, State};
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
check_dir_or_file(Path, #state{files = MapFiles} = State) ->
    {ok, RootDirList} = file:list_dir(Path),
    io:format("MAP: ~p~n", [MapFiles]),
    F = fun(File, MapFiles) ->
            FullPath = lists:concat([Path, '/', File]),
                case filelib:is_dir(FullPath) of
                    true ->
                        {ok, _} = supervisor:start_child(wmb_librarian_sup, [FullPath]),
                        MapFiles;
                    false ->
                        CheckResult = check_file(FullPath),
                        io:format("CheckResult: ~p~n", [CheckResult]),
                        case CheckResult of
                            {ok, {track_id, TrackID}} ->
                                MapNew = maps:new(),
                                LastMod = filelib:last_modified(FullPath),
                                MapTrack = maps:put(mtime, LastMod, MapNew),
                                io:format("FLAC: ~p~n", [[File, TrackID, MapFiles]]),
                                maps:put(File, maps:put(track_id, TrackID, MapTrack), MapFiles);
                            _ ->
                                MapFiles
                        end
                end
        end,
    MapFilesNew = lists:foldl(F, MapFiles, RootDirList),
    io:format("MapFilesNew: ~p~n", [[MapFilesNew, State]]),
    {ok, #state{path = Path, files = MapFilesNew}}.

-spec check_file(string()) ->
    {ok, flac} | {ok, cover} | {error, skip}.
check_file(FullPath) ->
    case re:run(FullPath, ".*.(flac)$", [caseless, unicode]) of
        {match, _} ->
            Result = wmb_digger:parse_file(fullpath, FullPath),
            %io:format("File for Check: ~p~n", [[FullPath, Result]]),
            %Self = self(),
            %Result = spawn(wmb_digger, parse_file, [fullpath, FullPath]),
            %io:format("File for Check: ~p~n", [[FullPath, Result]]),
            Result;
        nomatch ->
            %io:format("File Not Matched: ~p~n", [FullPath]),
            {error, skip}
    end.

