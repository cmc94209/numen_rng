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
-define(CRYPTO_CACHE_BITS, 56).

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

%%https://www.openssl.org/docs/man1.0.2/man3/BN_rand_range.html
%%file:///C:/Program%20Files/erl10.0/lib/crypto-4.3/doc/html/index.html
%%1.1 底层原理
%%系统随机性源： OpenSSL 首先会尝试获取系统提供的真随机数源，通常是操作系统的 /dev/random 或 /dev/urandom（在类 UNIX 系统中），或者是 Windows 的 CryptGenRandom 函数。这些源产生的数据是由系统硬件或操作系统的事件（例如用户的鼠标移动、键盘输入、硬件噪声等）产生的，具有高度的随机性。
%%
%%熵池（Entropy Pool）： OpenSSL 会维护一个称为熵池的数据结构，用于存储收集到的随机性源数据。这个熵池包含了足够的随机性数据，可以供 OpenSSL 的伪随机数生成器使用。
%%
%%伪随机数生成器（PRNG）： OpenSSL 使用一个伪随机数生成器（通常是一个加密安全的伪随机数生成器）来从熵池中生成更多的随机数据。这个 PRNG 的输出被 RAND_bytes() 函数返回给调用者。
%%
%%Re-seeding： 如果熵池中的随机性数据用尽，或者达到一定的使用次数，OpenSSL 会重新从系统的随机性源获取新的随机性数据，然后再将它们混合到熵池中，以维持熵池的随机性。
%%这个函数用于生成随机字节。它需要一个缓冲区和要生成的字节数作为参数。示例用法如下：
mk_alg() ->
%%    #{type => crypto,
%%        bits => 64,
%%        next => fun crypto:rand_plugin_next/1,
%%        uniform => fun crypto:rand_plugin_uniform/1,
%%        uniform_n => fun crypto:rand_plugin_uniform/2}.   %% BN_rand_range
    CacheBits = ?CRYPTO_CACHE_BITS,
    EnvCacheSize =
        application:get_env(
            crypto, rand_cache_size, CacheBits * 16), % Cache 16 * 8 words
    Bytes = (CacheBits + 7) div 8,
    CacheSize =
        case ((EnvCacheSize + (Bytes - 1)) div Bytes) * Bytes of
            Sz when is_integer(Sz), Bytes =< Sz ->
                Sz;
            _ ->
                Bytes
        end,
    {#{type => crypto,
        bits => CacheBits,
        next => fun crypto:rand_cache_plugin_next/1},
        {CacheBits, CacheSize, <<>>}}.