%%%-------------------------------------------------------------------
%% @doc wmb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wmb_librarian_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = transient,
    Shutdown = 5000,
    Type = worker,
    WmbLibrarian = {'wmb_librarian', {'wmb_librarian', start_link, []}, Restart, Shutdown, Type, [wmb_librarian]},
    {ok, {SupFlags, [WmbLibrarian]}}.

%%====================================================================
%% Internal functions
%%====================================================================
