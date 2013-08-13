-module(bqs_mob_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/bqs.hrl").

handle_call_get_stats_test() ->
    State = #mob_state{id = id,
                          weapon = weapon,
                          armor = armor},
    ?assertEqual({reply, {ok, {id, weapon, armor}}, State},
        bqs_mob:handle_call({get_stats}, from, State)).
