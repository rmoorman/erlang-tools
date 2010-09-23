%%% File        : ws_echo.erl
%%% Description : Websockets handler module demo.

-module(ws_echo).

%% websocket_mod behaviour functions
-export([init/1, shutdown/1, handle_data/2, handle_info/2]).

-behaviour(websocket_mod).

init(_) ->
    error_logger:info_msg("Initializing echo handler~n"),
    [].

shutdown(_State) ->
    error_logger:info_msg("Stopping echo handler~n"),
    ok.

handle_info({echo, Data}, State) ->
    {raw, Data, State};
handle_info(_, _State) ->
    ignore.

handle_data(Data, State) ->
    websocket_mod:send_message(self(), {echo, Data}),
    State.
