-module(first).

-export([main/1, readlines/1]).

main(Input) ->
    Matrix = readlines(Input),
    maps:fold(
        fun(_K, V, Acc) ->
            case V of
                Vn when Vn >= 2 -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        Matrix
    ).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, #{}).

middle_point_sequence(A, B, Module) ->
    lists:map(
        fun(Interval) ->
            middle_point(A, B, Interval / Module)
        end,
        lists:seq(1, Module - 1)
    ).

middle_point(
    {X1, Y1},
    {X2, Y2},
    K
) ->
    X12 = X1 + K * (X2 - X1),
    Y12 = Y1 + K * (Y2 - Y1),
    {float_to_int(X12), float_to_int(Y12)}.

float_to_int(Float) ->
    erlang:list_to_integer(
        erlang:float_to_list(Float, [{decimals, 0}])
    ).

module({X1, Y1}, {X2, Y2}) ->
    XPow2 = math:pow(X2 - X1, 2),
    YPow2 = math:pow(Y2 - Y1, 2),
    Result = math:sqrt(XPow2 + YPow2),
    Result.

origin_extreme(Line) ->
    [Part1, Part2] = binary:split(Line, [<<" -> ">>], [global]),
    {binary:split(Part1, [<<",">>], [global]), binary:split(Part2, [<<",">>], [global])}.

update_hits_in_matrix(Matrix, Segment, true) ->
    lists:foldl(
        fun(Point, MatrixN) ->
            PrevValue = maps:get(Point, MatrixN, 0),
            maps:put(Point, PrevValue + 1, MatrixN)
        end,
        Matrix,
        Segment
    );
update_hits_in_matrix(Matrix, _, _) ->
    Matrix.

segment(A, B, 1) ->
    [A, B];
segment(A, B, Module) ->
    MiddlePointsSequence = middle_point_sequence(A, B, Module),
    lists:flatten([A, MiddlePointsSequence, B]).

is_hor_or_ver({X1, _Y1}, {X2, _Y2}) when X1 == X2 -> true;
is_hor_or_ver({_X1, Y1}, {_X2, Y2}) when Y1 == Y2 -> true;
is_hor_or_ver(_, _) -> false.

parse([<<>>], Matrix) ->
    Matrix;
parse([Line | T], Matrix) ->
    {[X1, Y1], [X2, Y2]} = origin_extreme(Line),
    A = {parse_integer(X1), parse_integer(Y1)},
    B = {parse_integer(X2), parse_integer(Y2)},
    Module = float_to_int(module(A, B)),
    Segment = segment(A, B, Module),
    IsHorOrVer = is_hor_or_ver(A, B),
    io:format("A: ~w B: ~w || module: ~w~n", [A, B, IsHorOrVer]),

    NewMatrix = update_hits_in_matrix(Matrix, Segment, IsHorOrVer),
    parse(T, NewMatrix).

parse_integer(Bin) ->
    {Number, <<>>} = string:to_integer(Bin),
    Number.
