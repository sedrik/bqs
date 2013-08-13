-module(browserquest_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_ok/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    application:start(lager),
    lager:set_loglevel(lager_console_backend, debug),
    
    %% {Host, list({Path, Handler, Opts})}
    %% Dispatch the requests (whatever the host is) to
    %% erws_handler, without any additional options.
    ListeningPort = 
	case application:get_env(listening_port) of
	    undefined ->
		8000;
	    {ok, Port} ->
		Port
	end,

    Dispatch = [{'_', [
        {'_', browserquest_srv_handler, []}
    ]}],
    
%%    lager:debug("Starting browserquest_srv on port ~p", [ListeningPort]),

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    %% Listen in 10100/tcp for http connections.
    cowboy:start_listener(browserquest_srv_handler, 100,
        cowboy_tcp_transport, [{port, ListeningPort}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    browserquest_srv_sup:start_link().

stop(_State) ->
    ok.

%% internal ==============================================================
start_ok(App) -> start_ok(App, application:start(App, permanent)).

start_ok(_, ok) ->
    ok;
start_ok(_App, {error, {already_started, _App}}) ->
    ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start_ok(Dep),
    start_ok(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
