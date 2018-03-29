%% Feel free to use, reuse and abuse the code in this file.

%% @doc API handler
-module(api_handler).

-export([init/2]).
-export([terminate/3]).

-include("ets_names.hrl").

-define(API_DEFAULT_ITEMS, 12).

init(Req, Opts) ->
    Path = cowboy_req:path(Req),
    [_, _, APIType, APIid] = binary:split(Path, [<<"/">>], [global]),
    io:format("Path Elements: ~p~n", [[APIType, APIid]]),
    case APIType of
        <<"abc">> ->
            {ok, Letters} = data_merger:get_all_letters(),
            io:format("Response from /api/abc: ~p~n", [Letters]),
            Res = jsx:encode(Letters);
        <<"albums">> ->
            {ok, Album} = data_merger:get_album_by_albumid({album_id, binary_to_integer(APIid)}),
            io:format("Response from /api/albums/id: ~p~n", [Album]),
            Res = jsx:encode(Album);
        <<"artist">> ->
            {ok, Albums} = data_merger:get_albums_by_artistid({artist_id, binary_to_integer(APIid)}),
            io:format("Response from /api/artist/id: ~p~n", [Albums]),
            Res = jsx:encode(Albums);
        <<"letter">> ->
            {ok, Artists} = data_merger:get_artists_by_letterid({letter_id, binary_to_integer(APIid)}),
            io:format("Response from /api/letter/id: ~p~n", [Artists]),
            Res = jsx:encode(Artists);
        <<"date">> ->
            {ok, Albums} = data_merger:get_albums_by_date_tuple({date, APIid}),
            io:format("Response from /api/date/id: ~p~n", [Albums]),
            Res = jsx:encode(Albums);
        <<"dates">> ->
            {ok, Dates} = data_merger:get_all_dates(),
            io:format("Response from /api/dates/all: ~p~n", [Dates]),
            Res = jsx:encode(Dates);
        <<"genres">> ->
            {ok, Genres} = data_merger:get_all_genres(),
            io:format("Response from /api/genres/all: ~p~n", [Genres]),
            Res = jsx:encode(Genres);
        <<"page">> ->
            APIidAtom = binary_to_integer(APIid),
            SkipAlbums = (APIidAtom * ?API_DEFAULT_ITEMS) - ?API_DEFAULT_ITEMS,
            {ok, Albums} = data_merger:get_albums(tpl, SkipAlbums, ?API_DEFAULT_ITEMS),
            io:format("Response from /api/page/item: ~p~n", [Albums]),
            Res = jsx:encode(Albums);
        <<"random">> ->
            {ok, RandomTrackList} = data_merger:get_random_tracks(binary_to_integer(APIid)),
            io:format("Response from /api/random/id: ~p~n", [RandomTrackList]),
            Res = jsx:encode(RandomTrackList);
        <<"search">> ->
            {ok, Albums} = data_merger:search_artists_by_phrase(APIid),
            io:format("Response from /api/search/phrase: ~p~n", [Albums]),
            Res = jsx:encode(Albums);
        <<"tracks">> ->
            {ok, Track2Web} = data_merger:get_track_by_trackid({track_id, binary_to_integer(APIid)}),
            io:format("Response from /api/tracks/id: ~p~n", [Track2Web]),
            Res = jsx:encode(Track2Web);
        <<"albums_by_filter">> ->
            {ok, Body, _Req2} = cowboy_req:read_body(Req),
            Filters = jsx:decode(Body),
            {ok, Albums} = data_merger:get_albums_by_filters_v2(Filters),
            io:format("Response from /api/filters: ~p~n", [Albums]),
            Res = jsx:encode(Albums);
        _ ->
            Res = <<"API Request Not Found">>
    end,
    Req3 = cowboy_req:reply(200, #{
               <<"content-type">> => <<"application/json">>},
               Res, Req),
    {ok, Req3, Opts}.

terminate(_Reason, _Req, _Opts) ->
	ok.
