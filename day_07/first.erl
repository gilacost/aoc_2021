-module(first).

-export([main/1, fuel/3]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>, <<",">>], [global]),
    XPositionList = parse(BinSplit, []),
    Min = lists:min(XPositionList),
    Max = lists:max(XPositionList),
    PositionList = lists:seq(Min, Max),
    % para cada postion he de mirar la distancia que esta cada crab
    {FuelCostMap, _} = lists:foldl(
        fun(Position, {FuelPerPosition, _XPositionList}) ->
            Fuel = fuel(XPositionList, Position, []),
            {maps:put(Position, Fuel, FuelPerPosition), XPositionList}
        end,
        {initial_state(Min, Max), XPositionList},
        PositionList
    ),
    HandMadMax = 1000000000,

    Fun = fun(K, V, {Pos, FuelCost, MaxIn} = Acc) ->
        io:format("K:~p V:~w  POS:~w ~n", [K, V, Pos]),
        case V < MaxIn of
            true -> {K, FuelCost, V};
            _ -> Acc
        end
    end,
    maps:fold(Fun, {0, 0, HandMadMax}, FuelCostMap).

fuel([], _Position, Buffer) ->
    lists:sum(Buffer);
fuel([CrabPosition | T], Position, Buffer) ->
    FuelCost = increments(abs(CrabPosition - Position)),
    fuel(T, Position, [FuelCost | Buffer]).

increments(Increments) ->
    IncSeq = lists:seq(1, Increments),
    lists:foldl(
        fun(Inc, Acc) ->
            Inc + Acc
        end,
        0,
        IncSeq
    ).

initial_state(Min, Max) ->
    lists:foldl(
        fun(Index, State) ->
            maps:put(Index, 0, State)
        end,
        #{},
        lists:seq(Min, Max)
    ).

parse([<<>>], Buffer) ->
    lists:reverse(Buffer);
parse([<<H/binary>> | T], Buffer) ->
    {Number, <<>>} = string:to_integer(H),
    parse(T, [Number | Buffer]).
