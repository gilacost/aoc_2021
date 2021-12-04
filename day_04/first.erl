-module(first).

-export([main/1]).

main(Input) ->
    [ResultsUnparsed | BoardsUnparsed] = read_input(Input),
    Results = parse_results(ResultsUnparsed),
    Boards = parse_boards(BoardsUnparsed, [], 1),
    {BoardNumber, [Last | _T] = ResultsAcc} =
        catch lists:foldl(
            fun(Result, PostedResults) ->
                NewResults = [Result | PostedResults],
                case check_line_in_boards(Boards, NewResults) of
                    {_BoardNumber, _Results} = Match ->
                        throw(Match);
                    _ ->
                        NewResults
                end
            end,
            [],
            Results
        ),
    WinningBoardNumbers = lists:flatten(proplists:get_value(BoardNumber, Boards)),
    UnmarkedSumInBoard = lists:foldl(
        fun(BinNumber, Acc) -> parse_int(BinNumber) + Acc end, 0, WinningBoardNumbers -- ResultsAcc
    ),
    parse_int(Last) * UnmarkedSumInBoard.

parse_int(Bin) ->
    {Number, <<>>} = string:to_integer(Bin),
    Number.

check_line_in_boards([], _Results) ->
    false;
check_line_in_boards([{BoardNumber, Board} | T], Results) ->
    case check_lines(Board, Results) of
        true ->
            {BoardNumber, Results};
        _ ->
            check_line_in_boards(T, Results)
    end.

check_lines([], _Results) ->
    false;
check_lines([Line | T], Results) ->
    case length(Line -- Results) of
        0 -> true;
        _ -> check_lines(T, Results)
    end.

read_input(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n\n">>], [global]).

parse_results(Results) ->
    binary:split(Results, [<<",">>], [global]).

parse_boards([], Buffer, _Index) ->
    lists:reverse(Buffer);
parse_boards([<<B/binary>> | T], Boards, Index) ->
    Board = build_board(B),
    CleanedBoard = Board -- [[]],
    parse_boards(T, [{Index, CleanedBoard} | Boards], Index + 1).

build_board(BoardBin) ->
    Lines = binary:split(BoardBin, [<<"\n">>], [global]),
    parse_lines(Lines, []).

parse_lines([], Lines) ->
    lists:reverse(Lines);
parse_lines([LineBin | T], Lines) ->
    Line = binary:split(LineBin, [<<" ">>, <<"  ">>], [global]),
    CleanedLine = Line -- [<<>>],
    parse_lines(T, [CleanedLine | Lines]).
