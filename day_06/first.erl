-module(first).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>, <<",">>], [global]),
    io:format("1~p", [BinSplit]),
    BinSplitCleanned = lists:delete(<<>>, BinSplit),
    io:format("2~p", [BinSplitCleanned]),
    parse(BinSplit, []).

% split(Binary, By) ->
%     list_to_tuple(binary:split(Binary, By, [global])).

% build_acc(Length) ->
%     [{0, 0} || _X <- lists:seq(1, Length)].

% parse_integer(Bin) ->
%    {Number, <<>>} = string:to_integer(Bin),
%    Number.

parse([], Buffer) ->
    lists:reverse(Buffer);
parse([<<H/binary>> | T], Buffer) ->
    % {First, Second} = split(H, [<<" ">>]),
    case H of
        <<>> ->
            parse(T, [Buffer]);
        _ ->
            io:format("3~p", [H]),
            {Number, <<>>} = string:to_integer(H),
            parse(T, [Number | Buffer])
    end.
