-module(first).

-export([
    main/1,
    readlines/1,
    binary_list_to_decimal/1
]).

main(Input) ->
    Lines = [H | _T] = readlines(Input),
    Acc = build_acc(length(H)),
    CountBitsAcc = lists:foldl(fun gamma_rate/2, Acc, Lines),
    GammaRate = lists:map(fun do_most_common/1, CountBitsAcc),
    EpsilonRateInteger = binary_list_to_decimal(epsilon_rate(GammaRate)),
    GammaRateInteger = binary_list_to_decimal(GammaRate),
    erlang:list_to_integer(
        erlang:float_to_list(EpsilonRateInteger * GammaRateInteger, [{decimals, 0}])
    ).

gamma_rate(Bits, BitsAcc) ->
    lists:map(fun count/1, lists:zip(Bits, BitsAcc)).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, []).

build_acc(Length) ->
    [{0, 0} || _X <- lists:seq(1, Length)].

epsilon_rate(GammaRate) ->
    lists:map(fun opposite/1, GammaRate).

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

base("1") -> 2;
base("0") -> 0.

opposite("1") -> "0";
opposite("0") -> "1".

do_most_common({ZeroCount, OneCount}) when ZeroCount > OneCount -> "0";
do_most_common({ZeroCount, OneCount}) when ZeroCount < OneCount -> "1".

count({<<"0">>, {ZeroCount, OneCount}}) -> {ZeroCount + 1, OneCount};
count({<<"1">>, {ZeroCount, OneCount}}) -> {ZeroCount, OneCount + 1}.

parse([<<>>], Buffer) ->
    lists:reverse(Buffer);
parse([Line | T], Buffer) ->
    parse(T, [get_bits(Line, []) | Buffer]).

get_bits(<<>>, Buffer) ->
    lists:reverse(Buffer);
get_bits(<<Bit:1/binary, Rest/binary>>, Buffer) ->
    get_bits(Rest, [Bit | Buffer]).
