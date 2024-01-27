%%%-------------------------------------------------------------------
%%% @author jok1
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rng_generator_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => 'rng_generator',
        start => {'rng_generator', start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => ['rng_generator']},

    {ok, {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.
