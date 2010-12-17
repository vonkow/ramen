% TODO
% Prevent user from sending messages too quickly, add timestamp to userPID in state, check timestamp during post.
% Add timeout and char length check to received msgs
% Add /login, associate user name with PID (Check if name exists), prevent unlogged users from seeing chat.
% Add username and timestamp to posts.
% Add chatrooms, make sure user only gets messages from rooms they are in.
% Add message user.

-module(ramen).
-author('Caz').

-export([start/1, keepstate/1, sendloop/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
	register(st, spawn(ramen, keepstate, [[]])),
	listen(Port).

listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(LSocket).

accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	spawn(fun() -> 
		recvLoop(Socket) 
	end),
	addPID(spawn(ramen, sendloop, [Socket])),
	accept(LSocket).

recvLoop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			st ! {broadcast, Data},
			io:format("Message: ~s~n", [Data]),
			%timer:sleep(100),
			recvLoop(Socket);
		{error, closed} ->
			ok
	end.

sendloop(Socket) ->
	receive
		{send, M} ->
			case gen_tcp:send(Socket, M) of
				ok ->
					io:format("Sent ~w to ~w~n", [ok, self()]),
					sendloop(Socket);
				{error, _} ->
					io:format("Closed ~w~n", [self()]),
					% send call to keepstate to remove pid
					st ! {remove, self()},
					ok
			end
	end.

addPID(P) ->
	io:format("Pid ~w~n", [P]),
	st ! {add, P}.

bcast(M, []) -> 
	io:format("Sent: ~s~n", [M]);
bcast(M, State) ->
	[P | Rest] = State,
	io:format("Sending to ~w~n", [P]),
	P ! {send, M},
	bcast(M, Rest).

cull(P, State) ->
	lists:delete(P, State).

keepstate(State) ->
	receive
		{add, P} ->
			NewState = [P | State],
			io:format("State: ~w~n", [NewState]),
			keepstate(NewState);
		{remove, P} ->
			NewState = cull(P, State),
			io:format("Culled: ~w~n", [P]),
			keepstate(NewState);
		{broadcast, M} ->
			io:format("Got State: ~w~n", [State]),
			bcast(M, State),
			keepstate(State);
		quit ->
			ok
	end.

