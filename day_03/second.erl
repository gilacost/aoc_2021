-module(second).

-export([
    main/1,
    readlines/1,
    binary_list_to_decimal/1
]).

main(Input) ->
    Lines = [H | _T] = readlines(Input),

    EnabledOx = enabled(Lines, 1, oxigen),
    OxygenGeneratorRating = binary_list_to_decimal(
        walk_through(Lines, [], 1, length(H), EnabledOx, oxigen)
    ),

    EnabledCo2 = enabled(Lines, 1, co2),
    Co2GeneratorRating = binary_list_to_decimal(
        walk_through(Lines, [], 1, length(H), EnabledCo2, co2)
    ),
    OxygenGeneratorRating * Co2GeneratorRating.

enabled([H | _T] = Lines, Index, oxigen) ->
    Acc = build_acc(length(H)),
    CountBits = lists:foldl(fun count/2, Acc, Lines),
    lists:nth(Index, lists:map(fun most_common/1, CountBits));
enabled([H | _T] = Lines, Index, co2) ->
    Acc = build_acc(length(H)),
    CountBits = lists:foldl(fun count/2, Acc, Lines),
    lists:nth(Index, lists:map(fun less_common/1, CountBits)).

count(Bits, BitsAcc) ->
    lists:map(fun do_count/1, lists:zip(Bits, BitsAcc)).

do_count({<<"0">>, {ZeroCount, OneCount}}) -> {ZeroCount + 1, OneCount};
do_count({<<"1">>, {ZeroCount, OneCount}}) -> {ZeroCount, OneCount + 1}.

most_common({Zeroes, Ones}) when Zeroes > Ones -> <<"0">>;
most_common({Zeroes, Ones}) when Zeroes == Ones -> <<"1">>;
most_common(_) -> <<"1">>.

less_common({Zeroes, Ones}) when Zeroes > Ones -> <<"1">>;
less_common({Zeroes, Ones}) when Zeroes == Ones -> <<"0">>;
less_common(_) -> <<"0">>.

build_acc(Length) ->
    [{0, 0} || _X <- lists:seq(1, Length)].

walk_through([], Acc, Index, Length, _Enabled, Generator) ->
    case length(Acc) of
        1 ->
            lists:last(Acc);
        _ ->
            Enabled = enabled(Acc, Index + 1, Generator),
            walk_through(Acc, [], Index + 1, Length, Enabled, Generator)
    end;
walk_through([Bin | T], Acc, Index, Length, Enabled, Generator) ->
    case lists:nth(Index, Bin) of
        EnabledN when Enabled == EnabledN ->
            walk_through(T, [Bin | Acc], Index, Length, Enabled, Generator);
        _ ->
            walk_through(T, Acc, Index, Length, Enabled, Generator)
    end.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, []).

binary_list_to_decimal(BinList) ->
    BitsIndex = lists:reverse(lists:seq(0, length(BinList) - 1)),
    BinPowTuples = lists:zip(BitsIndex, BinList),
    lists:foldl(
        fun({Pow, Bit}, Acc) ->
            Base = base(Bit),
            case {Base, Pow} of
                {0, 0} -> Acc;
                _ -> Acc + math:pow(Base, Pow)
            end
        end,
        0,
        BinPowTuples
    ).

base(<<"1">>) -> 2;
base(<<"0">>) -> 0.

parse([<<>>], Buffer) ->
    lists:reverse(Buffer);
parse([Line | T], Buffer) ->
    parse(T, [get_bits(Line, []) | Buffer]).

get_bits(<<>>, Buffer) ->
    lists:reverse(Buffer);
get_bits(<<Bit:1/binary, Rest/binary>>, Buffer) ->
    get_bits(Rest, [Bit | Buffer]).
