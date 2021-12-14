-module(second).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = split(Data, [<<"\n">>]),
    IncompleteOrErrored = parse(BinSplit, []),
    Closings = [">", ")", "]", "}"],
    Incomplete = lists:filter(
        fun(Elem) -> not lists:member(Elem, Closings) end, IncompleteOrErrored
    ),
    Scores = lists:map(
        fun(Inc) ->
            lists:foldl(
                fun(H, Acc) ->
                    (Acc * 5) + close([H])
                end,
                0,
                lists:reverse(Inc)
            )
        end,
        Incomplete
    ),
    Index = ceil(length(Scores) / 2),
    lists:nth(Index, lists:sort(Scores)).

close("(") -> 1;
close("[") -> 2;
close("{") -> 3;
close("<") -> 4.

split(Binary, By) ->
    binary:split(Binary, By, [global]).

parse([<<>>], Line) ->
    lists:reverse(Line);
parse([<<H/binary>> | T], Line) ->
    BinList = binary_to_list(H),
    NewLine = catch parse_tokens(BinList, [], []),
    parse(T, [NewLine | Line]).

parse_tokens([], Final, _Errored) ->
    lists:reverse(Final);
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
