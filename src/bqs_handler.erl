%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Cowboy handler.
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_handler).
-behaviour(cowboy_websocket_handler).

-include("../include/bqs.hrl").

% Behaviour cowboy_websocket_handler
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3  
        ]).  

% Internal export
-export([make_tick/2]).

-define(APP, bqs).

-record(state, {
	  player,
	  tick_time
	 }).

%%%===================================================================
%%% API
%%%===================================================================
% Called to know how to dispatch a new connection.  
init({tcp, http}, Req, _Opts) ->  
    lager:debug("Request: ~p", [Req]),  
    % "upgrade" every request to websocket,  
    % we're not interested in serving any other content.
    {upgrade, protocol, cowboy_websocket}.

% Called for every new websocket connection.  
websocket_init(tcp, Req, []) ->  
    lager:debug("New client"),
    
    self() ! <<"Send gogo">>,

    {ok, TickTime} = application:get_env(?APP, tick_time),

    spawn(?MODULE, make_tick, [self(), TickTime]),

    {ok, Req, #state{tick_time = TickTime}}.  

websocket_handle({text, Msg}, Req, State) ->  
    Args = jiffy:decode(Msg),
    {Type, Reply, NewState} = parse_action(Args, State),
    
    case Type of
	json ->
	    self() ! {json, Reply}, Req, State,
	    {ok, Req, NewState};
	_ ->
	    {ok, Req, NewState}
    end;
  
% With this callback we can handle other kind of  
% messages, like binary.  
websocket_handle(_Any, Req, State) ->  
    bqs_util:unexpected_info(
      ?MODULE,"websocket binary received", State),
    {ok, Req, State}.  


% Called when a text message arrives. 
websocket_info(<<"Send gogo">>, Req, State) ->
    lager:debug("Sending 'go' message to client"),
    {reply, {text, <<"go">>}, Req, State};

websocket_info(<<"tick">>, Req, State = #state{player = undefined}) ->
    {ok, Req, State};

websocket_info(<<"tick">>, Req, State = #state{player = Player}) ->
    case bqs_player:get_surrondings(Player) of
	[] ->
	    ok;
	ActionList ->
	    lager:debug("Sending actionlist: ~p", [ActionList]),
	    self() ! {json, ActionList}
    end,
    {ok, Req, State};

websocket_info({json, Message}, Req, State) ->
    Json = jiffy:encode(Message),
    lager:debug("Sending json: ~p", [Json]),
    {reply, {text, Json}, Req, State};
  
websocket_info(Msg, Req, State) ->
    lager:debug("Got unknown message: ~p", [Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{player = Player}) ->      
    lager:debug("Connection closed"),
    bqs_player:stop(Player),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_action([?HELLO, Name, Armor, Weapon], State) ->
    %% This is a player call
    {ok, Player} = bqs_player:start_link(Name, Armor, Weapon),
    {ok, Status} = bqs_player:get_status(Player),
    {json, [?WELCOME|Status], State#state{player = Player}};

parse_action([?MOVE, X, Y], State = #state{player = Player}) ->
    {ok, Status} = bqs_player:move(Player, X, Y),
    lager:debug("MOVED ~p ~p", [X, Y]),
    {json, [?MOVE|Status], State};

parse_action([?ATTACK, Target], State = #state{player = Player}) ->
    ok = bqs_player:attack(Player, Target),
    {ok, [], State};

parse_action([?HIT, Target], State = #state{player = Player}) ->
    {ok, Return} = bqs_player:hit(Player, Target),

    {json, Return, State};

parse_action([?DAMAGE, _Target], State = #state{player = _Player}) ->
    {ok, [], State};

parse_action([?HURT, Attacker], State = #state{player = Player}) ->
    {ok, Status} = bqs_player:hurt(Player, Attacker),
    {json, Status, State};

parse_action([?AGGRO, _Target], State = #state{player = _Player}) ->
    {ok, [], State};

parse_action([?CHAT, Message], State = #state{player = Player}) ->
    {ok, Return} = bqs_player:chat(Player, Message),
    {json, Return, State};

parse_action([?TELEPORT, X, Y], State = #state{player = Player}) ->
    {ok, Status} = bqs_player:move(Player, X, Y),
    {json, [?TELEPORT|Status], State};

parse_action([?CHECK, Value], State = #state{player = Player}) ->
    bqs_player:set_checkpoint(Player, Value),
    {ok, [], State};

parse_action([?ZONE], State = #state{player = Player}) ->
    bqs_player:update_zone(Player),
    {ok, [], State};

parse_action(ActionList, _State) ->
    lager:error("Faulty actionlist: ~p", [ActionList]),
    exit({faulty_actionlist, ActionList}).

make_tick(Node, TickTime) ->
    Node ! <<"tick">>,
    receive 
	stop ->
	    ok
    after 
	TickTime ->
	    make_tick(Node, TickTime)
    end.

