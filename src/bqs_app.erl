-module(bqs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
        {'_', bqs_handler, []}
    ]}],
    
%%    lager:debug("Starting bqs on port ~p", [ListeningPort]),

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    %% Listen in 10100/tcp for http connections.
    ranch:start_listener(bqs_handler, 100,
        cowboy_tcp_transport, [{port, ListeningPort}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    bqs_sup:start_link().

stop(_State) ->
    ok.
