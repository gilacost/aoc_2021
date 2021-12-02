-module(second).

-export([main/1, readlines/1]).

main(Input) ->
    Lines = readlines(Input),
    {X, Y, _Z} = lists:foldl(fun do_action/2, {0, 0, 0}, Lines),
    abs(X * Y).

do_action({<<"up">>, Increment}, {X, Y, Z}) -> {X, Y, Z - Increment};
do_action({<<"down">>, Increment}, {X, Y, Z}) -> {X, Y, Z + Increment};
do_action({<<"forward">>, Increment}, {X, Y, Z}) -> {X + Increment, Y + (Z * Increment), Z}.

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
