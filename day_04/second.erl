-module(second).

-export([main/1]).

main(Input) ->
    [ResultsUnparsed | BoardsUnparsed] = read_input(Input),
    Results = parse_results(ResultsUnparsed),
    Boards = parse_boards(BoardsUnparsed, [], 1),
    Acc = lists:foldl(
        fun(Result, CalledNumbers) ->
            NewCalledNumbers = [Result | CalledNumbers],
            check_rows_in_boards(Boards, NewCalledNumbers, [])
        end,
        [],
        Results
    ),
    erlang:display(Acc).

check_rows_in_boards(Boards, CalledNumbers, WinningBoards) ->
    case
        catch maps:foreach(
            fun(BoardId, BoardLines) ->
                lists:foreach(
                    fun(Line) ->
                        erlang:display(Line),
                        case length(Line -- CalledNumbers) of
                            0 ->
                                erlang:display(es_zero),
                                NewBoardLines = lists:delete(Line, BoardLines),
                                throw({NewBoardLines, BoardId});
                            _ ->
                                Line
                        end
                    end,
                    BoardLines
                )
            end,
            Boards
        )
    of
        {NewBoardLines, BoardId} ->
            % TODO return new boards and update winning boards
            NewBoards = Boards#{BoardId := NewBoardLines},
            erlang:display(NewBoards),
            CalledNumbers;
        ok ->
            CalledNumbers
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
    proplists:to_map(parse_boards(T, [{Index, CleanedBoard} | Boards], Index + 1)).

build_board(BoardBin) ->
    Lines = binary:split(BoardBin, [<<"\n">>], [global]),
    parse_lines(Lines, []).

parse_lines([], Lines) ->
    lists:reverse(Lines);
parse_lines([LineBin | T], Lines) ->
    Line = binary:split(LineBin, [<<" ">>, <<"  ">>], [global]),
    CleanedLine = Line -- [<<>>],
    parse_lines(T, [CleanedLine | Lines]).
