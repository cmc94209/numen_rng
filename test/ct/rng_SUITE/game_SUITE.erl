%%%-------------------------------------------------------------------
%%% @author jok1
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 1月 2024 19:33
%%%-------------------------------------------------------------------
-module(game_SUITE).
-author("jok1").


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [
        make_game_grid
    ].

make_game_grid(_Config) ->
    {_, Grid} = ra_game_logic:make_game_grid(),
    ?assertEqual(true, length(Grid) == 15),
    ok.