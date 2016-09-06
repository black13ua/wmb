%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(welcome_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {ok, Res2} = albums_merger:get_albums(tpl),
    P1 = cowboy_req:qs(Req),
    io:format("Res2: ~p~n", [Res2]),
    io:format("Req: ~p~n", [P1]),
    {ok, Body} = welcome_dtl:render([
                    {name, "Johnny"},
                    {friends, [<<"Frankie Lee">>, <<"Judas Priest">>]},
                    {albums, Res2} 
                 ]),
	{ok, Req2} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"text/html">>}],
	               Body,
                   Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
