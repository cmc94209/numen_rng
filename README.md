# numen_rng
  ## Overview

>This project consists of three apps(rng ,book_of_ra,bingo). rng is responsible for generating random numbers,
> and book_of_ra will call rng's apps to generate the grid.
rng use **rand_algorithm:init_seed()**. Seed algorithm: **rng_generator:random_inner(N)**

## Quick Start
- You must have [Erlang/OTP 21](http://erlang.org/download.html)
OS:centos7
git clone https://github.com/cmc94209/numen_rng.git
- ./rebar3 as prod tar -----> rng-0.1.0.tar.gz ---> tar -xvf rng-0.1.0.tar.gz
- start in background: ./bin/rng start
- Start in foreground: ./bin/rng console
- More commands:./bin/rng help

## Core Module
- rand.erl
- rng_generator.erl
- ra_game_logic.erl
- rand_algorithm.erl

## interface description

### init seed
```erlang
    rand_algorithm:init_seed().
```

### gen random number
```erlang
    rand_algorithm:random(Range).
    rand_algorithm:random(Start,End).
```

## code segment
###  rand_algorithm
```erlang
init_seed(N) ->
    N8 = N * 8,
    <<I1:N8/unsigned-integer, I2:N8/unsigned-integer, I3:N8/unsigned-integer>> = crypto:strong_rand_bytes(N * 3),
    Alg = mk_alg(),
    Seed = {I1, I2, I3},
    rand:seed({Alg, Seed}).


mk_alg() ->
    #{type => crypto,
        bits => 64,
        next => fun crypto:rand_plugin_next/1,
        uniform => fun crypto:rand_plugin_uniform/1,
        uniform_n => fun crypto:rand_plugin_uniform/2}.
```


  


## Testing
    ./rebar3 ct