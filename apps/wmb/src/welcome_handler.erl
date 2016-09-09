%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(welcome_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ets_names.hrl").

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    %% Get needed Parameters
    AlbumsCount = ets:info(?ETS_ALBUMS, size),
    {LimitOnPage, _} = cowboy_req:qs_val(<<"limit">>, Req, <<"10">>),
    {CurrentPage, _} = cowboy_req:qs_val(<<"page">>, Req, <<"1">>),
    PagesCount = wmb_helpers:ceiling(AlbumsCount / erlang:binary_to_integer(LimitOnPage)),
    PagesList = lists:seq(1, PagesCount),
    SkipAlbums = erlang:binary_to_integer(CurrentPage) * erlang:binary_to_integer(LimitOnPage) - erlang:binary_to_integer(LimitOnPage),
    %%
    
    {ok, Res2} = albums_merger:get_albums(tpl, SkipAlbums, erlang:binary_to_integer(LimitOnPage)),

    io:format("Res2: ~p~n", [Res2]),
    io:format("Page Render: ~p~n", [[AlbumsCount, PagesCount, LimitOnPage, CurrentPage]]),
    {ok, Body} = welcome_dtl:render([
                    {albums_count, AlbumsCount},
                    {pages_count, PagesCount},
                    {pages_list, PagesList},
                    {limit_on_page, LimitOnPage},
                    {current_page, CurrentPage},
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
