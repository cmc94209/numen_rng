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
-export([seed/1]).

-export([uniform/0,uniform/1]).

%%% uniform/0, uniform/1, uniform_s/1, uniform_s/2 are all
%%% uniformly distributed random numbers.

%% uniform/0: returns a random float X where 0.0 < X < 1.0,
%% updating the state in the process dictionary.

-spec uniform() -> X :: float().
uniform() ->
    {X, Seed} = uniform_s(seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform/1: given an integer N >= 1,
%% uniform/1 returns a random integer X where 1 =< X =< N,
%% updating the state in the process dictionary.

-spec uniform(N :: pos_integer()) -> X :: pos_integer().
uniform(N) ->
    {X, Seed} = uniform_s(N, seed_get()),
    _ = seed_put(Seed),
    X.

%% uniform_s/1: given a state, uniform_s/1
%% returns a random float X where 0.0 < X < 1.0,
%% and a new state.

-spec uniform_s(state()) -> {X :: float(), NewS :: state()}.
uniform_s(State = {#{uniform := Uniform}, _}) ->
    Uniform(State).

%% uniform_s/2: given an integer N >= 1 and a state, uniform_s/2
%% uniform_s/2 returns a random integer X where 1 =< X =< N,
%% and a new state.

-spec uniform_s(N :: pos_integer(), state()) -> {X :: pos_integer(), NewS :: state()}.
uniform_s(N, State = {#{uniform_n := Uniform, max := Max}, _})
    when 0 < N, N =< Max ->
    Uniform(N, State);
uniform_s(N, State0 = {#{uniform := Uniform}, _})
    when is_integer(N), 0 < N ->
    {F, State} = Uniform(State0),
    {trunc(F * N) + 1, State}.


%% seed(Alg) seeds RNG with runtime dependent values
%% and return the NEW state

%% seed({Alg,Seed}) setup RNG with a previously exported seed
%% and return the NEW state

-spec seed(AlgOrExpState :: alg() | export_state()) -> state().
seed(Alg) ->
    R = seed_s(Alg),
    _ = seed_put(R),
    R.

-spec seed_s(AlgOrExpState :: alg() | export_state()) -> state().
seed_s(Alg) when is_atom(Alg) ->
    seed_s(Alg, {erlang:phash2([{node(), self()}]),
        erlang:system_time(),
        erlang:unique_integer()});
seed_s({Alg0, Seed}) ->
    {Alg, _SeedFun} = mk_alg(Alg0),
    {Alg, Seed}.

-spec seed_s(Alg :: alg(), {integer(), integer(), integer()}) -> state().
seed_s(Alg0, S0 = {_, _, _}) ->
    {Alg, Seed} = mk_alg(Alg0),
    AS = Seed(S0),
    {Alg, AS}.


seed_get() ->
    case get(?SEED_DICT) of
        undefined -> seed(exsplus);
        Old -> Old  % no type checking here
    end.


-spec seed_put(state()) -> undefined | state().
seed_put(Seed) ->
    put(?SEED_DICT, Seed).

%% Setup alg record
mk_alg(exsplus) ->
    {#{type => exsplus, max => ?UINT58MASK, next => fun exsplus_next/1,
        uniform => fun exsplus_uniform/1, uniform_n => fun exsplus_uniform/2},
        fun exsplus_seed/1}.


%% =====================================================================
%% exs64 PRNG: Xorshift64*
%% Algorithm by Sebastiano Vigna
%% Reference URL: http://xorshift.di.unimi.it/
%% =====================================================================
exsplus_uniform({Alg, R0}) ->
    {I, R1} = exsplus_next(R0),
    {I / (?UINT58MASK + 1), {Alg, R1}}.

exsplus_uniform(Max, {Alg, R}) ->
    {V, R1} = exsplus_next(R),
    {(V rem Max) + 1, {Alg, R1}}.



-dialyzer({no_improper_lists, exsplus_seed/1}).

exsplus_seed({A1, A2, A3}) ->
    {_, R1} = exsplus_next([(((A1 * 4294967197) + 1) band ?UINT58MASK) |
        (((A2 * 4294967231) + 1) band ?UINT58MASK)]),
    {_, R2} = exsplus_next([(((A3 * 4294967279) + 1) band ?UINT58MASK) |
        tl(R1)]),
    R2.

-dialyzer({no_improper_lists, exsplus_next/1}).

%% Advance xorshift116+ state for one step and generate 58bit unsigned integer
-spec exsplus_next(exsplus_state()) -> {uint58(), exsplus_state()}.
exsplus_next([S1 | S0]) ->
    %% Note: members s0 and s1 are swapped here
    S11 = (S1 bxor (S1 bsl 24)) band ?UINT58MASK,
    S12 = S11 bxor S0 bxor (S11 bsr 11) bxor (S0 bsr 41),
    {(S0 + S12) band ?UINT58MASK, [S0 | S12]}.