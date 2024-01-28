-module(bingo_game_logic).

-define(GAME_ID, bingo).
-export([speaker_reads_number/0]).


%% @doc
speaker_reads_number() ->
    lists:foldl(fun(SequenceId,{SpeakNumbers,LastNumberList})->
            L = length(LastNumberList),
        NumberIndex = rng_generator:random(?GAME_ID, L),
                Number = lists:nth(NumberIndex,LastNumberList),

        NewSpeakNumbers = [{SequenceId,Number}|SpeakNumbers],
        NewLastNumberList = LastNumberList -- [Number],
        {NewSpeakNumbers,NewLastNumberList}
                end,{[],number_list()},lists:seq(1,35)).

number_list()->
    lists:seq(1,75).




