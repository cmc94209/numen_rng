-module(ra_game_logic).

-define(GAME_ID, ra).

-export([make_game_grid/0,get_element_list/0]).

%% @doc generate grid
make_game_grid() ->
    %% This is a example GridShape data of one slot_game
    GridShape = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]],
    Fun = fun(PosList, {Column, IndexList, Result}) ->
        ElementList = get_element_list(),
        Index = rng_generator:random(?GAME_ID, 100),
        Func = fun(CurPos, {TmpIndex, TmpResult}) ->
            CurPosElem = lists:nth(TmpIndex, ElementList),
            Next = TmpIndex + 1,
            Next1 =
                case Next > 100 of
                    true -> 1;
                    false -> Next
                end,
            {Next1, [{CurPos, CurPosElem} | TmpResult]}
               end,
        {_, GridElem} = lists:foldl(Func, {Index, []}, PosList),
        ClientIndex = Index - 1,
        {Column + 1, [ClientIndex | IndexList], GridElem ++ Result}
          end,
    {_, IndexList, GridList} = lists:foldl(Fun, {1, [], []}, GridShape),
    {lists:reverse(IndexList), lists:sort(GridList)}.


%% This function will return element list length 100
%% In this module , will return a example data
get_element_list() ->
    [4,6,3,5,1,2,11,3,6,1,5,2,1,4,8,5,6,11,4,7,2,5,4,3,7,1,9,5,4,2,1,9,2,5,6,4,3,2,1,6,4,2,1,7,3,4,2,6,5,4,3,
        1,10,3,8,2,1,4,9,6,3,8,4,1,2,5,1,3,5,4,7,1,2,8,3,2,6,4,11,3,2,5,11,2,6,3,8,7,6,5,1,2,3,10,5,1,4,3,1,2].


