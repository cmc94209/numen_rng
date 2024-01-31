%%%-------------------------------------------------------------------
%%% @author jok1
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 1月 2024 18:17
%%%-------------------------------------------------------------------
-module(rand_algorithm).
-author("jok1").

-include("rng.hrl").

%% API

-export([init_seed/0, init_seed/1]).
-export([random/1, random/2]).


%% @doc
-spec init_seed(N :: integer()) -> rand:state().
init_seed(N) ->
    N8 = N * 8,
    <<I1:N8/unsigned-integer, I2:N8/unsigned-integer, I3:N8/unsigned-integer>> = crypto:strong_rand_bytes(N * 3),
    Alg = mk_alg(),
    Seed = {I1, I2, I3},
    rand:seed({Alg, Seed}).


-spec init_seed() -> rand:state().
init_seed() ->
    N = random(4, 10),
    init_seed(N).

%% @doc 在[1,Max]区间 随机一个整数
random(Max) when Max > 1 ->
    rand:uniform(Max).
%% @doc 在[Min,Max]区间 随机一个整数
random(1, Max) when Max > 1 ->
    rand:uniform(Max);
random(Same, Same) -> Same;
random(Min, Max) when Max > 1 ->
    M = Min - 1,
    rand:uniform(Max - M) + M.

mk_alg() ->
    #{type => crypto,
        bits => 64,
        next => fun crypto:rand_plugin_next/1,
        uniform => fun crypto:rand_plugin_uniform/1,
        uniform_n => fun crypto:rand_plugin_uniform/2}.