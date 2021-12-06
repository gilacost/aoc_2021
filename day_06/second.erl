-module(second).

-export([main/1]).

initial_state() ->
    lists:foldl(
        fun(Index, State) ->
            maps:put(Index, 0, State)
        end,
        #{},
        lists:seq(0, 8)
    ).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>, <<",">>], [global]),
    BinSplitCleanned = lists:delete(<<>>, BinSplit),
    InitialLanternFishs = parse(BinSplitCleanned, initial_state()),
    io:format("~p", [InitialLanternFishs]),
    Days = lists:seq(1, 256),
    Acc = lists:foldl(
        fun(_Day, State) ->
            new_day(State)
        end,
        InitialLanternFishs,
        Days
    ),
    io:format("~w~n", [Acc]),
    lists:sum(maps:values(Acc)).

new_day(
    #{
        8 := Day8Counter,
        7 := Day7Counter,
        6 := Day6Counter,
        5 := Day5Counter,
        4 := Day4Counter,
        3 := Day3Counter,
        2 := Day2Counter,
        1 := Day1Counter,
        0 := Day0Counter
    } = State
) ->
    State#{
        8 => Day0Counter,
        7 => Day8Counter,
        6 => Day0Counter + Day7Counter,
        5 => Day6Counter,
        4 => Day5Counter,
        3 => Day4Counter,
        2 => Day3Counter,
        1 => Day2Counter,
        0 => Day1Counter
    }.

counter(V) -> V + 1.

parse([], Buffer) ->
    Buffer;
parse([<<H/binary>> | T], Days) ->
    {Day, <<>>} = string:to_integer(H),
    parse(T, maps:update_with(Day, fun counter/1, Days)).
