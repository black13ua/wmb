%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(welcome_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {ok, Body} = welcome_dtl:render([
                    {name, "Johnny"},
                    {friends, [<<"Frankie Lee">>, <<"Judas Priest">>]},
%                    {primes, [1, 2, "3", <<"5">>]}
                    {primes, ets:first(albums)}
                 ]),
	{ok, Req2} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"text/plain">>}],
	               Body,
                   Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
