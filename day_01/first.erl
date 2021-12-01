-module(first).
-export([main/1, readlines/1]).
main(Input) ->
    Lines = readlines(Input),
    lists:foldl(
        fun(X, {Acc, Prev}) ->
            case X > Prev of
                true -> {Acc + 1, X};
                false -> {Acc, X}
            end
        end,
        {0, 0},
        Lines
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
