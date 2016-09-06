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
    {ok, FilesRoot} = application:get_env(wmb, files_root),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/",              toppage_handler, []},
			{"/welcome",       welcome_handler, []},
            {"/covers/[...]",  cowboy_static,   {dir, FilesRoot}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{ip,{0,0,0,0}},{port, 8080}], [
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
