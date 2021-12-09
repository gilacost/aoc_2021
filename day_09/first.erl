-module(first).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

parse([], Matrix, _Y, _X) ->
    Matrix;
parse([<<Line/binary>> | T], Matrix, X, Y) ->
    NewMatrix = parse_points(binary_to_list(Line), Matrix, X, Y),
    parse(T, NewMatrix, X, Y + 1).

parse_points([], Matrix, _X, _Y) ->
    Matrix;
parse_points([Val | T], Matrix, X, Y) ->
    NewMatrix = maps:put({X, Y}, parse_integer(Val), Matrix),
    parse_points(T, NewMatrix, X + 1, Y).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = lists:reverse(binary:split(Data, [<<"\n">>], [global])) -- [<<>>],
    Matrix = parse(BinSplit, #{}, 0, 0),
    Acc = maps:fold(
        fun({_X, _Y} = P, V, Acc) ->
            Top = top(P, Matrix),
            Right = right(P, Matrix),
            Left = left(P, Matrix),
            Bottom = bottom(P, Matrix),

            case {Top, Right, Left, Bottom, V} of
                {TopIn, RightIn, LeftIn, BottomIn, VIn} when
                    TopIn > VIn, RightIn > VIn, LeftIn > VIn, BottomIn > VIn
                ->
                    [VIn + 1 | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        Matrix
    ),
    lists:sum(Acc).

top({X, Y}, Map) ->
    maps:get({X, Y + 1}, Map, 10).

right({X, Y}, Map) ->
    maps:get({X + 1, Y}, Map, 10).

left({X, Y}, Map) ->
    maps:get({X - 1, Y}, Map, 10).

bottom({X, Y}, Map) ->
    maps:get({X, Y - 1}, Map, 10).

parse_integer(Bin) ->
    erlang:list_to_integer([Bin]).
