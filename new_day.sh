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

-export([main/1, readlines/1]).

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
