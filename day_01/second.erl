-module(second).
-export([main/1, readlines/1]).

main(Input) ->
    Lines = readlines(Input),
    Seq = lists:seq(1, length(Lines) - 3),
    lists:foldl(
        fun(Index, {Acc, {PrevA, PrevB, PrevC}}) ->
            A = lists:nth(Index, Lines),
            B = lists:nth(Index + 1, Lines),
            C = lists:nth(Index + 2, Lines),
            Prev = PrevA + PrevB + PrevC,
            Current = A + B + C,
            case Current > Prev of
                true -> {Acc + 1, {A, B, C}};
                false -> {Acc, {A, B, C}}
            end
        end,
        {0, {0, 0, 0}},
        Seq
    ).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, []).

parse([<<>>], Buffer) ->
    lists:reverse(Buffer);
parse([<<H/binary>> | T], Buffer) ->
    {Number, <<>>} = string:to_integer(H),
    parse(T, [Number | Buffer]).
