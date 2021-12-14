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
    BinSplit = binary:split(Data, [<<"\n">>], [global]) -- [<<>>],
    Matrix = parse(BinSplit, #{}, 0, 0),

    io:format("matrix: ~p~n ", [print_matrix(Matrix, 4, 4)]).

print_matrix(Matrix, X, Y) ->
    YInc = lists:seq(0, Y),
    XInc = lists:reverse(lists:seq(0, X)),
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
    erlang:list_to_integer([Bin]).

% step(Matrix) ->
%     maps:fold(
%         fun({X, Y} = P, V, MatrixIn) ->
%             case increment(V) of
%                 Vin when Vin > 9 -> zero({X, Y}, MatrixIn);
%                 _ -> maps:update_with(P, fun increment/1, MatrixIn)
%             end
%         end,
%         Matrix,
%         Matrix
%     ).

% zero({X, Y}, Matrix) ->
%     lists:foldl(
%         fun(Point, MatrixIn) ->
%             maps:update_with(Point, fun increment/1, MatrixIn)
%         end,
%         Matrix,
%         filter_out_of_bounds([
%             {X - 1, Y + 1},
%             {X, Y + 1},
%             {X + 1, Y + 1},
%             {X - 1, Y},
%             {X + 1, Y},
%             {X - 1, Y - 1},
%             {X, Y - 1},
%             {X + 1, Y - 1}
%         ])
%     ).

% filter_out_of_bounds(PointsList) ->
%     lists:filter(
%         fun({X, Y}) ->
%             (X > -1) and (X < 10) and (Y > -1) and (Y < 10)
%         end,
%         PointsList
%     ).

% increment(V) -> V + 1.
