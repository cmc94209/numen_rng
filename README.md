# numen_rng
  ## Overview

>This project consists of two apps, rng and book_of_ra. rng is responsible for generating random numbers, and book_of_ra will call rng's apps to
generate the grid.
rng use **exsplus**. Seed algorithm: **rand:seed_s(exsplus)**

## Quick Start
>You must have [Erlang/OTP 21](http://erlang.org/download.html)
OS:centos7
git clone https://github.com/cmc94209/numen_rng.git

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



## Testing
    ./rebar3 ct