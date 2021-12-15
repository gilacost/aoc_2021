-module(first).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

parse_points([], Matrix, MaxX, MaxY) ->
    {Matrix, MaxX, MaxY};
parse_points([Val | T], Matrix, MaxX, MaxY) ->
    [X, Y] = split(Val, [<<",">>]),
    PointX = parse_integer(X),
    PointY = parse_integer(Y),

    NewMatrix = maps:put({PointX, PointY}, "#", Matrix),
    parse_points(T, NewMatrix, max(MaxX, PointX), max(MaxY, PointY)).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    [Points, _Folds] = split(Data, [<<"\n\n">>]) -- [<<>>],
    PointsSplit = split(Points, [<<"\n">>]) -- [<<>>],
    {Matrix, X, Y} = parse_points(PointsSplit, #{}, 0, 0),
    MaxXY = max(Y, X),
    print_matrix(fill_with_dots(Matrix, MaxXY), MaxXY).

fill_with_dots(Matrix, MaxXY) ->
    YInc = lists:seq(0, MaxXY),
    XInc = lists:seq(0, MaxXY),
    io:format("~p ~p ~p ~n", [XInc, YInc, Matrix]),
    lists:foldl(
        fun(YIn, MatrixIn1) ->
            lists:foldl(
                fun(XIn, MatrixIn2) ->
                    Key = {XIn, YIn},
                    Filling = maps:get(Key, MatrixIn2, "."),
                    maps:put(Key, Filling, MatrixIn2)
                end,
                MatrixIn1,
                XInc
            )
        end,
        Matrix,
        YInc
    ).

split(Binary, By) ->
    binary:split(Binary, By, [global]).

print_matrix(Matrix, MaxXY) ->
    YInc = lists:seq(0, MaxXY),
    XInc = lists:reverse(lists:seq(0, MaxXY)),
    lists:map(
        fun(YIn) ->
            Line = lists:foldl(
                fun(XIn, Acc) ->
                    [maps:get({XIn, YIn}, Matrix) | Acc]
                end,
                [],
                XInc
            ),
            io:format("~p ~n", [Line])
        end,
        YInc
    ).

parse_integer(Bin) ->
    erlang:list_to_integer(binary:bin_to_list(Bin)).
