-module(first).

-export([main/1]).

-record(lantern_fish, {
    current_day = 8 :: non_neg_integer()
}).

main(Input) ->
    Lines = readlines(Input),
    erlang:display(Lines).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    BinSplit = binary:split(Data, [<<"\n">>, <<",">>], [global]),
    BinSplitCleanned = lists:delete(<<>>, BinSplit),
    InitialLanternFishs = parse(BinSplitCleanned, []),
    InitialState = #{newborns => 0, lantern_fish_list => InitialLanternFishs},
    Days = lists:seq(1, 256),
    #{lantern_fish_list := FinalLanternFishList} =
        lists:foldl(
            fun(_Day, #{lantern_fish_list := LanternFishList}) ->
                next_day(LanternFishList, #{newborns => 0, lantern_fish_list => []})
            end,
            InitialState,
            Days
        ),
    length(FinalLanternFishList).

append_lintern_fishs(List, none) -> List;
append_lintern_fishs(List, NewBorns) -> lists:append(List, NewBorns).

new_borns(N) when N > 0 ->
    [#lantern_fish{} || _X <- lists:seq(1, N)];
new_borns(_) ->
    none.

next_day([], #{lantern_fish_list := LanternFishList, newborns := NewBorns} = NLF) ->
    WithNewBorns = lists:reverse(append_lintern_fishs(LanternFishList, new_borns(NewBorns))),
    NLF#{lantern_fish_list := WithNewBorns};
next_day(
    [LanternFish | T],
    #{
        newborns := NewBorns,
        lantern_fish_list := LanternFishList
    } = NLF
) ->
    NewLanternFishs =
        case new_day(LanternFish) of
            {true, NewLanternFish} ->
                NLF#{
                    newborns := NewBorns + 1,
                    lantern_fish_list := [NewLanternFish | LanternFishList]
                };
            {false, NewLanternFish} ->
                NLF#{
                    lantern_fish_list := [NewLanternFish | LanternFishList]
                }
        end,
    next_day(T, NewLanternFishs).

new_day(#lantern_fish{current_day = CurrentDay} = Lf) when
    CurrentDay < 9, CurrentDay > 0
->
    {false, Lf#lantern_fish{current_day = CurrentDay - 1}};
new_day(#lantern_fish{current_day = 0} = Lf) ->
    {true, Lf#lantern_fish{current_day = 6}}.

is_first_day(8) ->
    true;
is_first_day(_) ->
    false.

parse([], Buffer) ->
    Buffer;
parse([<<H/binary>> | T], Buffer) ->
    {Day, <<>>} = string:to_integer(H),
    LanternFish = #lantern_fish{current_day = Day, is_first_day = is_first_day(Day)},
    parse(T, [LanternFish | Buffer]).
