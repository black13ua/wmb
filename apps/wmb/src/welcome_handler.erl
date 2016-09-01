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
    io:format("Res2: ~p~n", [Res2]),
    %%% for cover.jpg
    %%% filename:dirname("/home/black/my/mtest/Rodrigo Amarante/2013 - Cavalo/06 - Fall Asleep.flac")
%%%%     AlbumTuple = ets:first(wmb_albums),
%%%%     [{AlbumTuple, AlbumID}|_] = ets:lookup(wmb_albums, AlbumTuple),
%%%%     [{AlbumID, GenreTuple}] = ets:lookup(wmb_genres, AlbumID),
%%%%     TracksList = ets:match(wmb_tracks, {AlbumID, {'$2', '$1'}}),
%%%%     %TracksList = ets:lookup(wmb_tracks, AlbumID),
%%%%
%%%%    AlbumList = erlang:tuple_to_list(AlbumTuple),
%%%%    %TracksList = erlang:tuple_to_list(TracksTuple),
%%%%
%%%%    Res1 = [GenreTuple|AlbumList],
%%%%    Res2 = [{tracks, TracksList} | Res1],
%%%%    io:format("Result: ~p~n", [Res2]),

    {ok, Body} = welcome_dtl:render([
                    {name, "Johnny"},
                    {friends, [<<"Frankie Lee">>, <<"Judas Priest">>]},
                    {albums, Res2 
                             % [{artist, <<"Vijay Iyer">>}, {album, <<"Echo Deutscher Musikpreis Jazz 2013">>},
                             %  {date, <<"2013">>}, {genre, <<"Jazz">>},
                             %  {tracks, [<<"Track1">>, <<"Track2">>, <<"Track3">>, <<"Track4">>, <<"Track5">>]}
                             % ],
                             % [{artist, <<"Till Bronner">>}, {album, <<"At The End Of The Day">>},
                             %  {date, <<"2010">>}, {genre, <<"Jazz">>},
                             %  {tracks, [<<"Everybody's Got To Learn">>, <<"And I Love Her">>, <<"Some Other Things">>]}
                             % ]
                    }
%                    {albums, [AlbumTuple, Genre1, Tracks1]}
                 ]),
	{ok, Req2} = cowboy_req:reply(
                   200,
                   [{<<"content-type">>, <<"text/html">>}],
	               Body,
                   Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
