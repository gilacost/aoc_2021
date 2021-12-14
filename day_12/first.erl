-module(first).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, #{}).

split(Binary, By) ->
    binary:split(Binary, By, [global]).

parse([<<>>], Map) ->
    Map;
parse([<<H/binary>> | T], Map) ->
    [Origin, Dst] = split(H, [<<"-">>]),
    UpdatedMap = insert(Origin, Dst, Map),
    parse(T, UpdatedMap).

insert(From, To, Map) ->
    FromList = maps:get(From, Map, []),
    FromDict = maps:put(From, lists:usort([To | FromList]), Map),
    ToList = maps:get(To, Map, []),
    maps:put(To, lists:usort([From | ToList]), FromDict).

% continue(Acc, Visited, Dest) =
%   if is_big(Cave) of

is_big(Cave) -> string:to_upper(Cave) == Cave.
