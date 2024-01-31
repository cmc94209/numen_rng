-module(bg_trace_helper).

-include("bg_base_common.hrl").

%%-ifdef(TEST).
%%-define(TRACE_LOG(Msg), ct:pal(Msg)).
%%-define(TRACE_LOG(Format, Args), ct:pal(Format, Args)).
%%-else.
%%-define(TRACE_LOG(Msg), lager:debug(Msg)).
%%-define(TRACE_LOG(Format, Args), lager:debug(Format, Args)).
%%-endif.

%% API
-export([
    trace/1, trace/2, trace/3, trace/4,
    trace_pid/1, trace_pid/2, trace_pid/3, trace_pid/4, trace_pid/5,
    do_trace/2,
    get_tspec/1,
    clear_all/0
]).

-type tspecs() :: [recon_trace:tspec()].
-type options() :: [{trace_count, integer()} | {formatter, all} | recon_trace:options()].
-type fun_name() :: atom().
-type args() :: list() | '_' | arity().

-spec trace(module() | tspecs()) -> recon_trace:num_matches().
trace(Mod) when is_atom(Mod) ->
    trace(Mod, '_');
trace(TSpecs) when is_list(TSpecs) ->
    trace(TSpecs, []).

-spec trace(tspecs(), options()) -> recon_trace:num_matches();
    (module(), options()) -> recon_trace:num_matches();
    (module(), fun_name()) -> recon_trace:num_matches().
trace(TSpecs0, Options) when is_list(TSpecs0) andalso is_list(Options) ->
    TSpecs = [get_tspec(TSpec) || TSpec <- TSpecs0],
    do_trace(TSpecs, Options);
trace(Mod, Options) when is_atom(Mod) andalso is_list(Options) ->
    trace(Mod, '_', '_', Options);
trace(Mod, Fun) when is_atom(Mod) andalso is_atom(Fun) ->
    trace(Mod, Fun, '_', []).

-spec trace(module(), fun_name(), args()) -> recon_trace:num_matches().
trace(Mod, Fun, Args) when is_atom(Mod) andalso is_atom(Fun) ->
    trace(Mod, Fun, Args, []).

-spec trace(module(), fun_name(), args(), options()) -> recon_trace:num_matches().
trace(Mod, Fun, Arity, Options) when is_atom(Mod) andalso is_atom(Fun) andalso is_integer(Arity) ->
    trace(Mod, Fun, lists:duplicate(Arity, '_'), Options);
trace(Mod, Fun, Args, Options) when is_atom(Mod) andalso is_atom(Fun) ->
    do_trace([{Mod, Fun, [{Args, [], [{return_trace}]}]}], Options).

-spec trace_pid(recon_trace:pidspec()) -> recon_trace:num_matches().
trace_pid(Pid) ->
    trace_pid(Pid, '_').

-spec trace_pid(recon_trace:pidspec(), module()) -> recon_trace:num_matches();
    (recon_trace:pidspec(), tspecs()) -> recon_trace:num_matches().
trace_pid(Pid, Mod) when is_atom(Mod) ->
    trace_pid(Pid, Mod, '_');
trace_pid(Pid, TSpecs) when is_list(TSpecs) ->
    trace_pid(Pid, TSpecs, []).

-spec trace_pid(recon_trace:pidspec(), tspecs(), options()) -> recon_trace:num_matches();
    (recon_trace:pidspec(), module(), options()) -> recon_trace:num_matches();
    (recon_trace:pidspec(), module(), fun_name()) -> recon_trace:num_matches().
trace_pid(Pid, TSpecs0, Options) when is_list(TSpecs0) andalso is_list(Options) ->
    TSpecs = [get_tspec(TSpec) || TSpec <- TSpecs0],
    do_trace_pid(Pid, TSpecs, Options);
trace_pid(Pid, Mod, Options) when is_atom(Mod) andalso is_list(Options) ->
    trace_pid(Pid, Mod, '_', '_', Options);
trace_pid(Pid, Mod, Fun) when is_atom(Mod) andalso is_atom(Fun) ->
    trace_pid(Pid, Mod, Fun, '_', []).

-spec trace_pid(recon_trace:pidspec(), module(), fun_name(), args()) -> recon_trace:num_matches().
trace_pid(Pid, Mod, Fun, Args) when is_atom(Mod) andalso is_atom(Fun) ->
    trace_pid(Pid, Mod, Fun, Args, []).

-spec trace_pid(recon_trace:pidspec(), module(), fun_name(), args(), options()) -> recon_trace:num_matches().
trace_pid(Pid, Mod, Fun, Arity, Options) when is_atom(Mod) andalso is_atom(Fun) andalso is_integer(Arity) ->
    trace_pid(Pid, Mod, Fun, lists:duplicate(Arity, '_'), Options);
trace_pid(Pid, Mod, Fun, Args, Options) when is_atom(Mod) andalso is_atom(Fun) ->
    do_trace_pid(Pid, [{Mod, Fun, [{Args, [], [{return_trace}]}]}], Options).

-spec do_trace_pid(recon_trace:pidspec(), tspecs(), options()) -> recon_trace:num_matches().
do_trace_pid(Pid, TSpecs, Options) when is_list(TSpecs) ->
    do_trace(TSpecs, [{pid, Pid} | Options]).

-spec do_trace(tspecs(), options()) -> recon_trace:num_matches().
do_trace(TSpecs, Options0) when is_list(TSpecs) ->
    [code:ensure_loaded(Mod) || {Mod, _, _} <- TSpecs, Mod =/= '_' andalso is_atom(Mod)],
    {TraceCount, Options1} =
        case lists:keytake(trace_count, 1, Options0) of
            false -> {100, Options0};
            {value, {_, Count}, OptionsT1} ->
                {Count, OptionsT1}
        end,
    Options =
        case proplists:get_value(to_file, Options1) of
            undefined -> Options1;
            true ->
                {ok, Dev} = file:open("trace_msg.log", [write]),
                io:format("trace_file Dev:~p~n", [Dev]),
                put(trace_file, Dev),
                [{io_server, Dev} | Options1];
            Filename ->
                {ok, Dev} = file:open(Filename, [write]),
                io:format("trace_file Dev:~p~n", [Dev]),
                put(trace_file, Dev),
                [{io_server, Dev} | Options1]
        end,
    recon_trace:calls(TSpecs, TraceCount, [{scope, local} | Options]).

get_tspec(Mod) when is_atom(Mod) ->
    {Mod, '_', [{'_', [], [{return_trace}]}]};
get_tspec({Mod, Fun}) when is_atom(Mod) andalso is_atom(Fun) ->
    {Mod, Fun, [{'_', [], [{return_trace}]}]};
get_tspec({Mod, Fun, Arity}) when is_atom(Mod) andalso is_atom(Fun) andalso is_integer(Arity) ->
    {Mod, Fun, [{lists:duplicate(Arity, '_'), [], [{return_trace}]}]};
get_tspec({_Mod, _Fun, [{_, _, [{return_trace}]} | _]} = TSpec) -> TSpec;
get_tspec({Mod, Fun, Args}) when is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) ->
    {Mod, Fun, [{Args, [], [{return_trace}]}]}.

clear_all() ->
    case erlang:erase(trace_file) of
        undefined -> ignore;
        Dev ->
            file:close(Dev)
    end,
    recon_trace:clear().