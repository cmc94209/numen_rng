%%%-------------------------------------------------------------------
%% @doc rng top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rng_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->


    AChild = #{id => 'rng_generator_sup',
        start => {'rng_generator_sup', start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => supervisor,
        modules => ['rng_generator_sup']},


    {ok, {{one_for_all, 0, 1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
