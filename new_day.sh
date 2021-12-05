#!/bin/bash
DAY=$(printf "%02d" $1)
DIR=day_$DAY
SESSION_ID=${2:-$AOC_SESSION_ID}

echo $DAY
echo $DIR
echo $SESSION_ID

create ()
{
cat > $1 <<EOF
-module($2).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>], [global]),
    parse(BinSplit, []).

% ocurrences(List, El, string) -> length([X || X <- List, [X] =:= El]),

% even_print([]) ->
%     [];

% even_print([H | T]) when H rem 2 /= 0 ->
%     even_print(T);

% even_print([H | T]) ->
%     io:format("printing: ~p~n", [H]),
%     [H | even_print(T)].

% split(Binary, By) ->
%     list_to_tuple(binary:split(Binary, By, [global])).

% build_acc(Length) ->
%     [{0, 0} || _X <- lists:seq(1, Length)].

% binary_list_to_decimal(BinList) ->
%     BitsIndex = lists:reverse(lists:seq(0, length(BinList) - 1)),
%     BinPowTuples = lists:zip(BitsIndex, BinList),
%     lists:foldl(
%         fun({Pow, Bit}, Acc) ->
%             Base = base(Bit),
%             case {Base, Pow} of
%                 {0, 0} -> Acc;
%                 _ -> Acc + math:pow(Base, Pow)
%             end
%         end,
%         0,
%         BinPowTuples
%     ).

% get_bits(<<Bit:1/binary, Rest/binary>>, Buffer) ->
%   get_bits(Rest, [Bit | Buffer]).

% parse_integer(Bin) ->
%    {Number, <<>>} = string:to_integer(Bin),
%    Number.

% erlang:list_to_integer(
%     erlang:float_to_list(EpsilonRateInteger * GammaRateInteger, [{decimals, 0}])
% ).

%middle_point(
%   {X1, Y1},
%    {X2, Y2},
%    K,
%    Slope
%) ->
%    X12 = X1 + K * (X2 - X1),
%    Y12 = Y1 + K * (Y2 - Y1),
%    normalise({X12, Y12}, Slope).

% float_to_int(Float) ->
%     erlang:list_to_integer(
%         erlang:float_to_list(Float, [{decimals, 0}])
%     ).

%normalise({X, Y}, M) when M == 1 ->
%    {
%        float_to_int(math:ceil(X)),
%        float_to_int(math:ceil(Y))
%    };
%normalise({X, Y}, M) when M == -1 ->
%    {
%        float_to_int(math:ceil(X)),
%        float_to_int(math:floor(Y))
%    };
%normalise({X, Y}, _) ->
%    {
%        float_to_int(X),
%        float_to_int(Y)
%    }.

% module({X1, Y1}, {X2, Y2}) ->
%     XPow2 = math:pow(X2 - X1, 2),
%     YPow2 = math:pow(Y2 - Y1, 2),
%     math:sqrt(XPow2 + YPow2).

%origin_extreme(Line) ->
%    [Part1, Part2] = binary:split(Line, [<<" -> ">>], [global]),
%    {binary:split(Part1, [<<",">>], [global]), binary:split(Part2, [<<",">>], [global])}.

% remove_dups([]) -> [];
% remove_dups([H | T]) -> [H | [X || X <- remove_dups(T), X /= H]].

% remove_dups([]) -> [];
% remove_dups([H | T]) -> [H | [X || X <- remove_dups(T), X /= H]].

parse([<<>>], Buffer) ->
    lists:reverse(Buffer);
parse([<<H/binary>> | T], Buffer) ->
    % {First, Second} = split(H, [<<" ">>]),
    {Number, <<>>} = string:to_integer(H),
    parse(T, [Number | Buffer]).
EOF
}

mkdir -p $DIR
create $DIR/first.erl "first"
create $DIR/second.erl "second"
cd $DIR

curl "https://adventofcode.com/2021/day/$1/input" \
  -H "cookie: session=$SESSION_ID" \
  --compressed -O input
