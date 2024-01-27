-module(rng_ct_hook).

%% API
-export([
    init/2,
    terminate/1
]).

init(Id, _Opts) ->
    application:ensure_all_started(rng),
    application:ensure_all_started(ra),
    {ok, Id}.

terminate(_) ->
    ok = application:stop(rng),
    ok = application:stop(ra),
    ok.
