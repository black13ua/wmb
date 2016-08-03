%%%-------------------------------------------------------------------
%% @doc wmb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wmb_sup).

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
%%%%    {ok, { {one_for_all, 0, 1}, []} }.
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 5000,
    Type = worker,
    AChild = {'wmb_srv', {'wmb_srv', start_link, []}, Restart, Shutdown, Type, [wmb_srv]},
    {ok, {SupFlags, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
