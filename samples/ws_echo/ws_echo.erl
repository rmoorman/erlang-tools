%%% File        : ws_echo.erl
%%% Description : Websockets handler module demo.

-module(ws_echo).

%% websocket_mod behaviour functions
-export([init/1, shutdown/0, handle_data/1, handle_event/1]).

-behaviour(websocket_mod).

init(_) ->
    error_logger:info_msg("Initializing echo handler~n"),
    ok.

shutdown() ->
    error_logger:info_msg("Stopping echo handler~n"),
    ok.

handle_event({echo, Data}) ->
    {raw, Data};
handle_event(_) ->
    ignore.

handle_data(Data) ->
    self() ! {echo, Data}.
