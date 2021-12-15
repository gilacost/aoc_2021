-module(first).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = split(Data, [<<"\n">>]),
    lists:sum(
        lists:map(
            fun(H) ->
                points([H])
            end,
            lists:flatten(parse(BinSplit, []))
        )
    ).

points(")") -> 3;
points("]") -> 57;
points("}") -> 1197;
points(">") -> 25137.

split(Binary, By) ->
    binary:split(Binary, By, [global]).

parse([<<>>], Line) ->
    lists:reverse(Line);
parse([<<H/binary>> | T], Line) ->
    BinList = binary_to_list(H),
    NewLine = catch parse_tokens(BinList, [], []),
    parse(T, [NewLine | Line]).

parse_tokens([], _Final, _Errored) ->
    "";
parse_tokens([H | T], [], []) ->
    NewList = [H] ++ [],
    parse_tokens(T, NewList, []);
parse_tokens([H | T], Final, Errored) ->
    FinalLen = length(Final),
    NewList = [H] ++ Final,
    Prev = string:slice(lists:reverse(Final), FinalLen - 1),
    {NewListIn, NewErrored} =
        case closes(Prev, [H]) of
            closes ->
                {string:slice(NewList, 2), Errored};
            still_open ->
                {NewList, Errored};
            breaks ->
                throw([H])
        end,
    io:format("Slice: ~p New List: ~p closes: ~p ~n ", [
        Prev, lists:reverse(NewList), closes(Prev, [H])
    ]),
    parse_tokens(T, NewListIn, NewErrored).

closes("(", ")") ->
    closes;
closes("{", "}") ->
    closes;
closes("[", "]") ->
    closes;
closes("<", ">") ->
    closes;
closes("<", "}") ->
    breaks;
closes("<", "]") ->
    breaks;
closes("<", ")") ->
    breaks;
closes("(", "}") ->
    breaks;
closes("(", "]") ->
    breaks;
closes("(", ">") ->
    breaks;
closes("{", ")") ->
    breaks;
closes("{", "]") ->
    breaks;
closes("{", ">") ->
    breaks;
closes("[", ")") ->
    breaks;
closes("[", "}") ->
    breaks;
closes("[", ">") ->
    breaks;
closes(_, _) ->
    still_open.
