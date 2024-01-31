%%%-------------------------------------------------------------------
%%% @author jok1
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rng_generator).

-behaviour(gen_server).

-include("rng.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([
    seed_get/1
]).
-export([random/2, random/3]).

-define(SERVER, ?MODULE).

-record(state, {game_id}).

pid(GameId) ->
    case whereis(GameId) of
        undefined ->
            supervisor:start_child(rng_generator_sup, [GameId]);
        Pid ->
            {ok, Pid}
    end.


-spec seed_get(game_id()) -> undefined | state().
seed_get(GameId) ->
    {ok, Pid} = pid(GameId),
    gen_server:call(Pid, seed_get).

%% Generate a (1-M) random number for the game
-spec random(game_id(), integer()) -> integer().
random(GameId, N) when is_integer(N) ->
    {ok, Pid} = pid(GameId),
    gen_server:call(Pid, {random, N}).

%% Generate a (N-M) random number for the game
-spec random(game_id(), integer(), integer()) -> integer().
random(GameId, N, M) when is_integer(N), is_integer(M) ->
    {ok, Pid} = pid(GameId),
    gen_server:call(Pid, {random, N, M}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link(GameId) ->
    gen_server:start_link(?MODULE, [GameId], []).

init([GameId]) ->
    erlang:register(GameId, self()),
    rand_algorithm:init_seed(),
    {ok, #state{game_id = GameId}}.

handle_call({random, N}, _From, State = #state{}) ->
    RandValue = random_inner(N),
    {reply, RandValue, State};
handle_call({random, N, M}, _From, State = #state{}) ->
    RandValue = random_inner(N, M),
    {reply, RandValue, State};
handle_call({seed_put, Seed}, _From, State = #state{}) ->
    {reply, rand:seed_put(Seed), State};
handle_call(seed_get, _From, State = #state{}) ->
    {reply, erlang:get(rand_seed), State};

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @doc 在[1,Max]区间 随机一个整数
random_inner(Max) when Max > 1 ->
    rand:uniform(Max).
%% @doc 在[Min,Max]区间 随机一个整数
random_inner(1, Max) when Max > 1 ->
    rand:uniform(Max);
random_inner(Same, Same) -> Same;
random_inner(Min, Max) when Max > 1 ->
    M = Min - 1,
    rand:uniform(Max - M) + M.


