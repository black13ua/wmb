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

-record(state, {path = undefined, timeout = undefined, dirs = [], files = #{}}).

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
    self() ! scan,
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
handle_info(scan, #state{path = Path} = State) ->
    {ok, NewState} = scan_directory(Path, State),
    {ok, RescanTimeout} = application:get_env(wmb, rescan_timeout),
    DelayRandom = crypto:rand_uniform(1, 30000),
    Timeout = RescanTimeout * 1000 + DelayRandom,
    timer:send_after(Timeout, rescan),
    {noreply, NewState#state{timeout = Timeout}, hibernate};
handle_info(rescan, #state{path = Path, timeout = Timeout} = State) ->
%    io:format("Rescan Path/State: ~p~n", [[Path, State]]),
    case scan_directory(Path, State) of
        {ok, NewState} ->
            %io:format("Rescan NewState: ~p~n", [[Path, NewState]]),
            timer:send_after(Timeout, rescan),
            {noreply, NewState, hibernate};
        {error, _} ->
            io:format("There is we stop worker! ~p~n", [Path]),
            {stop, normal, State}
    end;
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
-spec scan_directory(string(), #state{}) ->
    {ok, #state{}}.
scan_directory(Path, #state{dirs = StateDirs, files = StateFilesMap} = State) ->
    case find_dirs_and_files(Path) of
        {ok, {Files, Dirs}} ->
            ResDir = check_dir_scaned(Path, Dirs, StateDirs),
            ResFile = check_files_scaned(Path, Files, StateFilesMap),
            {ok, State#state{path = Path, dirs = ResDir, files = ResFile}};
        {error, _} ->
            {error, dir_error}
    end.

-spec find_dirs_and_files(string()) ->
    {list(), list()}.
find_dirs_and_files(Path) ->
    case file:list_dir(Path) of
        {ok, List} ->
            %io:format("Path Found: ~p~n", [Path]),
            Fun = fun(File, {Files, Dirs}) ->
                      FullPath = lists:concat([Path, '/', File]),
                      case filelib:is_dir(FullPath) of
                          true ->
                              {Files, [File|Dirs]};
                          false ->
                              case re:run(FullPath, ".*.(flac)$", [caseless, unicode]) of
                                  {match, _} ->
                                      {[File|Files], Dirs};
                                  nomatch ->
                                      {Files, Dirs}
                              end
                      end
                  end,
            Res = lists:foldl(Fun, {[],[]}, List),
            {ok, Res};
        {error, Error} ->
            io:format("Path not Found: ~p~n", [Path]),
            {error, Error}
    end.

-spec check_dir_scaned(string(), list(), list()) ->
    list().
check_dir_scaned(Path, Dirs, StateDirs) ->
    FunD = fun(Dir, Acc) ->
               %io:format("Dir and Dirs: ~p~n", [[Dir, StateDirs]]),
               case lists:member(Dir, StateDirs) of
                   true ->
                       %io:format("Dir scaned: ~p~n", [Dir]),
                       [Dir|Acc];
                   false ->
                       FullPath = lists:concat([Path, '/', Dir]),
                       {ok, Pid} = supervisor:start_child(wmb_librarian_sup, [FullPath]),
                       io:format("Dir not scaned: ~p~n", [[FullPath, Pid]]),
                       [Dir|Acc]
               end
          end,
    lists:foldl(FunD, [], Dirs).

-spec check_files_scaned(string(), list(), map()) ->
    list().
check_files_scaned(Path, Files, StateFilesMap) ->
    StateFiles = maps:keys(StateFilesMap),
    FunF = fun(File, Acc) ->
               %io:format("File for SCAN: ~p~n", [File]),
               case lists:member(File, StateFiles) of
                   true ->
                       %io:format("File scaned: ~p~n", [File]),
                       Val = maps:get(File, StateFilesMap),
                       maps:put(File, Val, Acc);
                   false ->
                       FullPath = lists:concat([Path, '/', File]),
                       FlacAddRes = wmb_digger:parse_file(fullpath, FullPath),
                       case FlacAddRes of
                           {ok, {track_id, TrackID}} ->
                               MapNew = maps:new(),
                               MTime = filelib:last_modified(FullPath),
                               MapTrack = maps:put(mtime, MTime, MapNew),
                               maps:put(File, maps:put(track_id, TrackID, MapTrack), Acc);
                           _ ->
                               Acc
                       end
               end
          end,
    lists:foldl(FunF, #{}, Files).

