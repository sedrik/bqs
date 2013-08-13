-module(browserquest_srv_app_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    browserquest_srv_app:start_ok(browserquest_srv),
    ok.

teardown(ok) ->
    application:stop(browserquest_srv),
    application:unload(browserquest_srv).

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
    Expected = lists:member({browserquest_srv,
                             permanent}, Started),
    [?_assertEqual(true, Expected)].
