-module(bqs_app_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    bqs_app:start_ok(bqs),
    %% This sleep is not needed to make the test pass but prevents a crash due
    %% to the system not having time to start completley before the terminate
    %% function
    timer:sleep(10),
    ok.

teardown(ok) ->
    application:stop(bqs),
    application:unload(bqs).

is_started_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun (Args) ->
             [
              is_started_(Args)
             ]
     end
    }.

is_started_(_) ->
    Started = proplists:get_value(started, application:info()),
    Expected = lists:member({bqs, permanent}, Started),
    [?_assertEqual(true, Expected)].
