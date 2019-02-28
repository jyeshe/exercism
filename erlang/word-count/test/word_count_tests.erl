%% Based on canonical data version 1.2.0
%% https://github.com/exercism/problem-specifications/raw/master/exercises/word-count/canonical-data.json
%% This file is automatically generated from the exercises canonical data.

-module(word_count_tests).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").


assertCount(Exp0, Actual0) ->
    Exp1=lists:sort(maps:to_list(Exp0)),
    Actual1=lists:sort(maps:to_list(Actual0)),
    ?assertMatch(Exp1, Actual1).


'1_count_one_word_test'() ->
    assertCount(#{"word" => 1},
		word_count:count_words("word")).

'2_count_one_of_each_word_test'() ->
    assertCount(#{"each" => 1, "of" => 1, "one" => 1},
		word_count:count_words("one of each")).

'3_multiple_occurrences_of_a_word_test'() ->
    assertCount(#{"blue" => 1, "fish" => 4, "one" => 1,
		  "red" => 1, "two" => 1},
		word_count:count_words("one fish two fish red fish blue fish")).

'4_handles_cramped_lists_test'() ->
    assertCount(#{"one" => 1, "three" => 1, "two" => 1},
		word_count:count_words("one,two,three")).

'5_handles_expanded_lists_test'() ->
    assertCount(#{"one" => 1, "three" => 1, "two" => 1},
		word_count:count_words("one,\ntwo,\nthree")).

'6_ignore_punctuation_test'() ->
    assertCount(#{"as" => 1, "car" => 1, "carpet" => 1,
		  "java" => 1, "javascript" => 1},
		word_count:count_words("car: carpet as java: javascript!!&@$%^&")).

'7_include_numbers_test'() ->
    assertCount(#{"1" => 1, "2" => 1, "testing" => 2},
		word_count:count_words("testing, 1, 2 testing")).

'8_normalize_case_test'() ->
    assertCount(#{"go" => 3, "stop" => 2},
		word_count:count_words("go Go GO Stop stop")).

'9_with_apostrophes_test'() ->
    assertCount(#{"cry" => 1, "don't" => 2, "first" => 1,
		  "laugh" => 1, "then" => 1},
		word_count:count_words("First: don't laugh. Then: don't cry.")).

'10_with_quotations_test'() ->
    assertCount(#{"and" => 1, "between" => 1, "can't" => 1,
		  "joe" => 1, "large" => 2, "tell" => 1},
		word_count:count_words("Joe can't tell between 'large' and large.")).

'11_multiple_spaces_not_detected_as_a_word_test'() ->
    assertCount(#{"multiple" => 1, "whitespaces" => 1},
		word_count:count_words(" multiple   whitespaces")).