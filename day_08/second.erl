-module(second).

-export([main/1]).

main(Input) ->
    Lines = readlines(Input),
    lists:sum(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = split(Data, [<<"\n">>]),
    parse_line(BinSplit, [], 1).

decipher(String) when length(String) == 2 -> {String, 1};
decipher(String) when length(String) == 3 -> {String, 7};
decipher(String) when length(String) == 4 -> {String, 4};
decipher(String) when length(String) == 7 -> {String, 8};
decipher(_String) -> none.

split(Binary, By) -> binary:split(Binary, By, [global]).

parse_line([<<>>], LinesMap, _LineNum) ->
    LinesMap;
parse_line([<<RawLine/binary>> | T], LinesMap, LineNum) ->
    [SignalsRaw, ResultRaw] = split(RawLine, <<" | ">>),
    Signals = split(SignalsRaw, <<" ">>),
    Results = lists:map(
        fun(Bin) ->
            lists:sort(binary_to_list(Bin))
        end,
        split(ResultRaw, <<" ">>)
    ),
    Uniq = get_unique(Signals, #{}),
    WithComplex = get_complex(Signals, Uniq),
    MappedResult = lists:map(
        fun(Result) ->
            integer_to_list(maps:get(Result, WithComplex))
        end,
        Results
    ),
    FinalResult = list_to_integer(string:join(MappedResult, "")),
    parse_line(T, [FinalResult | LinesMap], LineNum + 1).

decipher_complex(String, 4, 3, 3) when length(String) == 5 -> {String, 2};
decipher_complex(String, 3, 2, 2) when length(String) == 5 -> {String, 3};
decipher_complex(String, 4, 2, 3) when length(String) == 5 -> {String, 5};
decipher_complex(String, 4, 3, 3) when length(String) == 6 -> {String, 0};
decipher_complex(String, 5, 3, 4) when length(String) == 6 -> {String, 6};
decipher_complex(String, 4, 2, 3) when length(String) == 6 -> {String, 9};
decipher_complex(_String, _, _, _) -> none.

get_complex([], Buffer) ->
    Buffer;
get_complex([Signal | T], #{1 := OneStr, 4 := FourStr, 7 := SevenStr} = Buffer) ->
    SignalStr = binary_to_list(Signal),

    MinusOneLen = length(SignalStr -- OneStr),
    MinusFourLen = length(SignalStr -- FourStr),
    MinusSevenLen = length(SignalStr -- SevenStr),

    NewBuffer =
        case decipher_complex(SignalStr, MinusOneLen, MinusFourLen, MinusSevenLen) of
            none -> Buffer;
            {String, Value} -> maps:put(lists:sort(String), Value, Buffer)
        end,
    get_complex(T, NewBuffer).

get_unique([], Buffer) ->
    Buffer;
get_unique([Signal | T], Buffer) ->
    NewBuffer =
        case decipher(binary_to_list(Signal)) of
            none ->
                Buffer;
            {String, Value} ->
                NewBufferIn = maps:put(lists:sort(String), Value, Buffer),
                maps:put(Value, String, NewBufferIn)
        end,
    get_unique(T, NewBuffer).
