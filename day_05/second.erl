-module(second).

-export([main/1]).

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

middle_point_sequence(A, B, Module, Slope) ->
    lists:map(
        fun(Interval) ->
            middle_point(A, B, Interval / Module, Slope)
        end,
        lists:seq(1, Module)
    ).

middle_point({X1, Y1}, {X2, Y2}, K, Slope) ->
    X12 = X1 + K * (X2 - X1),
    Y12 = Y1 + K * (Y2 - Y1),
    normalise({X12, Y12}, Slope).

float_to_int(Float) ->
    erlang:list_to_integer(
        erlang:float_to_list(Float, [{decimals, 0}])
    ).

normalise({X, Y}, M) when M == 1 ->
    {
        float_to_int(math:ceil(X)),
        float_to_int(math:ceil(Y))
    };
normalise({X, Y}, M) when M == -1 ->
    {
        float_to_int(math:ceil(X)),
        float_to_int(math:floor(Y))
    };
normalise({X, Y}, _) ->
    {float_to_int(X), float_to_int(Y)}.

normalise_module(Float, _) ->
    {Integer, Decimal} = string:to_integer(
        erlang:float_to_list(Float, [{decimals, 1}])
    ),
    case string:to_integer(string:slice(Decimal, 1)) of
        {Int, []} when Int > 5 -> Integer + 1;
        _ -> Integer
    end.

module({X1, Y1}, {X2, Y2}) ->
    XPow2 = math:pow(X2 - X1, 2),
    YPow2 = math:pow(Y2 - Y1, 2),
    math:sqrt(XPow2 + YPow2).

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

segment(A, B, 1, _Slope) ->
    [A, B];
segment(A, B, Module, Slope) ->
    MiddlePointsSequence = middle_point_sequence(A, B, normalise_module(Module, Slope), Slope),
    remove_dups(lists:flatten([A, MiddlePointsSequence, B])).

is_valid_direction({X1, Y1}, {X2, Y2}, _) when X1 == X2; Y1 == Y2 -> true;
is_valid_direction({_X1, Y1}, {_X2, Y2}, _) when Y1 == Y2 -> true;
is_valid_direction(_A, _B, M) when M == 1; M == -1 -> true;
is_valid_direction(_A, _B, _M) -> false.

remove_dups([]) -> [];
remove_dups([H | T]) -> [H | [X || X <- remove_dups(T), X /= H]].

% m is the slope
slope({X1, Y1}, {X2, Y2}) ->
    case {Y2 - Y1, X2 - X1} of
        {_Y, 0} -> undefined;
        {_Y, -0} -> undefined;
        {Y, X} -> trunc(Y / X)
    end.

parse([<<>>], Matrix) ->
    Matrix;
parse([Line | T], Matrix) ->
    {[X1, Y1], [X2, Y2]} = origin_extreme(Line),
    A = {parse_integer(X1), parse_integer(Y1)},
    B = {parse_integer(X2), parse_integer(Y2)},
    Module = module(A, B),
    Slope = slope(A, B),
    Segment = segment(A, B, Module, Slope),
    IsValidDirection = is_valid_direction(A, B, Slope),

    NewMatrix = update_hits_in_matrix(Matrix, Segment, IsValidDirection),
    parse(T, NewMatrix).

parse_integer(Bin) ->
    {Number, <<>>} = string:to_integer(Bin),
    Number.
