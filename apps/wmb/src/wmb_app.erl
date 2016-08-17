%%%-------------------------------------------------------------------
%% @doc wmb public API
%% @end
%%%-------------------------------------------------------------------

-module(wmb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% Start Cowboy
    %ok = application:start(ranch),
    %ok = application:start(cowlib),
    %ok = application:start(cowboy),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/",           toppage_handler, []},
			{"/welcome",    welcome_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    %%
    wmb_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
