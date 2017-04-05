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
            {"/",             toppage_handler, []},
            {"/api/[...]",    api_handler,     []},
            {"/welcome",      welcome_handler, []},
            {"/files/[...]",  cowboy_static, {dir, FilesRoot}},
            {"/static/[...]", cowboy_static, {priv_dir, wmb, "www/static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{ip, {0,0,0,0}}, {port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    %%
    Res = wmb_sup:start_link(),
    {ok, _} = supervisor:start_child(wmb_librarian_sup, [FilesRoot]),
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
