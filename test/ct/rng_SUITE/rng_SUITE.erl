-module(rng_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

init_per_suite(Config) ->
    GameId = lion,
    RandRange = {1, 100},
    rng_generator:seed_get(GameId),
    [{game_id, GameId}, {rand_range, RandRange} | Config].

end_per_suite(_Config) ->
    ok.

all() ->
    [
        random_rng
    ].

random_rng(Config) ->
    GameId = proplists:get_value(game_id, Config),
    {Min, Max} = proplists:get_value(rand_range, Config),
    RandValue = rng_generator:random(GameId, Min, Max),
    ?assertEqual(true,erlang:is_integer(RandValue)),
    ?assertEqual(true,RandValue >= Min andalso RandValue =< Max),
    ok.