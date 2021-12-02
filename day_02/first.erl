-module(first).

-export([main/1, readlines/1]).

main(Input) ->
    Lines = readlines(Input),
    {X, Y} = lists:foldl(
        fun(Instruction, Coordinates) ->
            do_action(Instruction, Coordinates)
        end,
        {0, 0},
        Lines
    ),
    abs(X * Y).

do_action({<<"up">>, Increment}, {X, Y}) -> {X, Y + Increment};
do_action({<<"down">>, Increment}, {X, Y}) -> {X, Y - Increment};
do_action({<<"forward">>, Increment}, {X, Y}) -> {X + Increment, Y}.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, []).

parse([<<>>], Buffer) ->
    lists:reverse(Buffer);
parse([<<H/binary>> | T], Buffer) ->
    {Action, Moves} = split(H, [<<" ">>]),
    {MovesInteger, <<>>} = string:to_integer(Moves),
    parse(T, [{Action, MovesInteger} | Buffer]).

split(Binary, By) ->
    list_to_tuple(binary:split(Binary, By, [global])).
