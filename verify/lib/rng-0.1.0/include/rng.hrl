-define(DEFAULT_ALG_HANDLER, exsplus).
-define(SEED_DICT, rand_seed).

-define(UINT21MASK, 16#00000000001fffff).
-define(UINT32MASK, 16#00000000ffffffff).
-define(UINT33MASK, 16#00000001ffffffff).
-define(UINT39MASK, 16#0000007fffffffff).
-define(UINT58MASK, 16#03ffffffffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

-type uint64() :: 0..16#ffffffffffffffff.
-type uint58() :: 0..16#03ffffffffffffff.

-type game_id() :: atom().

-type alg() :: exs64 | exsplus | exs1024.
-type exs64_state() :: uint64().
-type exsplus_state() :: nonempty_improper_list(uint58(), uint58()).
-type exs1024_state() :: {list(uint64()), list(uint64())}.


%% =====================================================================
%% Types
%% =====================================================================

%% This depends on the algorithm handler function
-type alg_seed() :: exs64_state() | exsplus_state() | exs1024_state().
%% This is the algorithm handler function within this module
-type alg_handler() :: #{type := alg(),
max := integer(),
next := fun(),
uniform := fun(),
uniform_n := fun()}.

-opaque state() :: {alg_handler(), alg_seed()}.

-opaque export_state() :: {alg(), alg_seed()}.
-export_type([alg/0, state/0, export_state/0]).