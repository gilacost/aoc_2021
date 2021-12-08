-module(first).

-export([main/1]).

-define(NUMBER_SEGMENTS_MAP, #{
    2 => 1,
    3 => 7,
    4 => 4,
    5 => {2, 3, 5},
    6 => {0, 6, 9},
    7 => 8
}).
main(Input) ->
    #{1 := Ones, 4 := Fours, 7 := Sevens, 8 := Eights} = ResultsState = readlines(Input),
    io:format("~w ~n", [ResultsState]),
    Ones + Fours + Sevens + Eights.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = split(Data, [<<"\n">>]),
    parse_line(BinSplit, initial_state()).

initial_state() ->
    #{
        1 => 0,
        7 => 0,
        4 => 0,
        {2, 3, 5} => 0,
        {0, 6, 9} => 0,
        8 => 0
    }.

split(Binary, By) ->
    binary:split(Binary, By, [global]).

parse_line([<<>>], StateMap) ->
    StateMap;
parse_line([<<RawLine/binary>> | T], StateMap) ->
    [_Signals, ResultRaw] = split(RawLine, <<" | ">>),
    io:format("Result raw: ~w ~n", [ResultRaw]),
    Result = split(ResultRaw, <<" ">>),
    io:format("Result raw: ~w ~n", [Result]),
    NewStateMap = parse_result(Result, StateMap),

    parse_line(T, NewStateMap).

parse_result([], StateMap) ->
    StateMap;
parse_result([Result | T], StateMap) ->
    Key = maps:get(length(binary_to_list(Result)), ?NUMBER_SEGMENTS_MAP),
    io:format("key: ~p for:~w ~n", [Key, Result]),
    NumberCount = maps:get(Key, StateMap),
    NewStateMap = maps:put(Key, NumberCount + 1, StateMap),
    parse_result(T, NewStateMap).

% -define(NUMBER_SEGMENTS_MAP, #{
%     2 => 1,
%     3 => 7,
%     4 => 4,
%     5 => {2, 3, 5},
%     6 => {0, 6, 9},
%     7 => 8
% }).
% initial_state() ->
%     #{
%         1 => 0,
%         7 => 0,
%         4 => 0,
%         {2, 3, 5} => 0,
%         {0, 6, 9} => 0,
%         8 => 0
%     }.
