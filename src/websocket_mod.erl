%%% File    : websocket_mod.erl
%%% Author  : Dominique Boucher <>
%%% Description : Websockets behaviour
%%% Created :  9 Mar 2010 by Dominique Boucher <>

-module(websocket_mod).


-export([out/1, send_event/1, send_event/2]).
-export([behaviour_info/1]).


-include("/home/dboucher/lib/yaws/include/yaws_api.hrl").


-define(HANDLER_MODULE_PARAMETER, "websocket_handler").


behaviour_info(callbacks) ->
    [{init, 1}, {handle_data, 2}, {handle_event, 2}, {shutdown, 1}];
behaviour_info(_Other) ->
    undefined.


send_event(Event) ->
    send_event(self(), Event).

send_event(WsProcess, Event) ->
    WsProcess ! {ws_event, Event}.


out(A) -> 
    case get_upgrade_header(A#arg.headers) of 
	undefined ->
	    error_logger:info_msg("Receive a request from a non-websocket client~n"),
	    {content, "text/plain", "You're not a web sockets client! Go away!"};
	"WebSocket" ->
	    case get_handler(A) of
		{module, HandlerModule} ->
		    WebSocketOwner = spawn(fun() -> init_handler(HandlerModule, A) end),
		    {websocket, WebSocketOwner, passive};
		_ ->
		    {content, "text/plain", "Undefined websocket handler module"}
	    end
    end.


get_handler(Arg) ->
    Opaque = Arg#arg.opaque,
    case lists:keysearch(?HANDLER_MODULE_PARAMETER, 1, Opaque) of
	{value, {_, Value}} ->
	    HandlerModule = list_to_atom(Value),
	    {module, HandlerModule};
	_ ->
	    error
    end.


init_handler(HandlerModule, HttpArgs) ->
    receive
	{ok, WebSocket} ->
	    yaws_api:websocket_setopts(WebSocket, [{active, true}]),
	    State = apply({HandlerModule, init}, [HttpArgs]),
	    event_loop(HandlerModule, WebSocket, State);
	_ -> 
	    apply({HandlerModule, shutdown}, [[]]),
	    ok
    end.

event_loop(HandlerModule, WebSocket, State) ->
    receive
	{tcp, WebSocket, DataFrame} ->
	    Data = yaws_api:websocket_unframe_data(DataFrame),
	    NewState = apply({HandlerModule, handle_data}, [Data, State]),
            event_loop(HandlerModule, WebSocket, NewState);
	{tcp_closed, WebSocket} ->
	    apply({HandlerModule, shutdown}, [State]),
	    bye;
	{ws_event, Event} ->
	    case apply({HandlerModule, handle_event}, [Event, State]) of
		{json, JsonObject, NewState} ->
		    Data = list_to_binary(json:encode(JsonObject)),
		    yaws_api:websocket_send(WebSocket, Data),
		    event_loop(HandlerModule, WebSocket, NewState);
		{raw, Data, NewState} when is_binary(Data) ->
		    yaws_api:websocket_send(WebSocket, Data),
		    event_loop(HandlerModule, WebSocket, NewState);
		ignore ->
		    event_loop(HandlerModule, WebSocket, State);
		Result ->
		    error_logger:warning_msg("Unhandled result in websocket handler: ~p", [Result]),
		    event_loop(HandlerModule, WebSocket, State)
	    end
    end.

get_upgrade_header(#headers{other=L}) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            "upgrade" ->
                                V;
                            _ ->
                                undefined
                        end;
                   (_, Acc) ->
                        Acc
                end, undefined, L).
