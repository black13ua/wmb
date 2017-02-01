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
    {Filter_genre, _} = cowboy_req:qs_val(<<"genre">>, Req),
    PagesCount = wmb_helpers:ceiling(AlbumsCount / erlang:binary_to_integer(LimitOnPage)),
    PagesList = lists:seq(1, PagesCount),
    SkipAlbums = erlang:binary_to_integer(CurrentPage) * erlang:binary_to_integer(LimitOnPage) - erlang:binary_to_integer(LimitOnPage),
    GenresList = lists:usort(lists:flatten(ets:match(?ETS_GENRES, {'_',{genre,'$2'}}))),
    DatesList = lists:usort(lists:flatten(ets:match(?ETS_ALBUMS, {{{album, '_'}, {date, '$1'}}, {album_id, '_'}}))),
    %%
    
    {ok, Res2} = data_merger:get_albums(tpl, SkipAlbums, erlang:binary_to_integer(LimitOnPage)),

    io:format("Res2: ~p~n", [Res2]),
    io:format("Page Render: ~p~n", [[AlbumsCount, PagesCount, LimitOnPage, CurrentPage]]),
    io:format("Genre is: ~p~n", [Filter_genre]),
    {ok, Body} = welcome_dtl:render([
                    {albums_count, AlbumsCount},
                    {pages_count, PagesCount},
                    {pages_list, PagesList},
                    {limit_on_page, LimitOnPage},
                    {current_page, CurrentPage},
                    {genres_list, GenresList},
                    {dates_list, DatesList},
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
