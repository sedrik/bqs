-module(browserquest_srv_mob_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/browserquest.hrl").

handle_call_get_stats_test() ->
    State = #mob_state{id = id,
                          weapon = weapon,
                          armor = armor},
    ?assertEqual({reply, {ok, {id, weapon, armor}}, State},
        browserquest_srv_mob:handle_call({get_stats}, from, State)).
