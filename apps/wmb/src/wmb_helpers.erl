-module(wmb_helpers).
-export([ceiling/1, get_rel_path_by_dirname/1, skip_ets_elements/2, split_path_and_filename/1]).


%%%%
% https://erlangcentral.org/wiki/index.php/Floating_Point_Rounding
%%%%
-spec ceiling(number()) ->
    integer().
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

-spec skip_ets_elements(integer(), atom()) ->
    list() | term().
skip_ets_elements(0, Ets) ->
    Key = ets:first(Ets),
    Key;
skip_ets_elements(Skip, Ets) when is_integer(Skip), is_atom(Ets) ->
    Key = ets:first(Ets),
    skip_ets_elements(Skip, Ets, Key).

-spec skip_ets_elements(integer(), atom(), term()) ->
    list() | term().
skip_ets_elements(0, _Ets, Key) ->
    Key;
skip_ets_elements(Skip, Ets, Key) when is_integer(Skip), is_atom(Ets) ->
    Next = ets:next(Ets, Key),
    case Next of
        '$end_of_table' ->
            {error, end_of_table};
        _ ->
            skip_ets_elements(Skip - 1, Ets, Next)
    end.

-spec split_path_and_filename(string()) ->
    {ok, {bitstring(), bitstring()}}.
split_path_and_filename(File) ->
    {ok, FileRel} = get_rel_path_by_filename(File),
    FileBasename = unicode:characters_to_binary(filename:basename(FileRel)),
    AlbumPathRelBin = unicode:characters_to_binary(filename:dirname(FileRel)),
    {ok, {AlbumPathRelBin, FileBasename}}.

-spec get_rel_path_by_filename(string()) ->
    {ok, string()}.
get_rel_path_by_filename(File) ->
    {ok, Root} = application:get_env(wmb, files_root),
    FPathList = filename:split(File),
    SLL = length(FPathList),
    SRL = length(filename:split(Root)),
    RFileList = lists:sublist(FPathList, SRL+1, SLL+1),
    {ok, filename:join(RFileList)}.

-spec get_rel_path_by_dirname(string()) ->
    {ok, bitstring()}.
get_rel_path_by_dirname(Dir) ->
    {ok, Root} = application:get_env(wmb, files_root),
    FPathList = filename:split(Dir),
    SLL = length(FPathList),
    SRL = length(filename:split(Root)),
    RFileList = lists:sublist(FPathList, SRL+1, SLL+1),
    {ok, unicode:characters_to_binary(filename:join(RFileList))}.
