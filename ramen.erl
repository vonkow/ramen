% TODO
% Prevent user from sending messages too quickly, add timestamp to userPID in state, check timestamp during post.
% Add timeout and char length check to received msgs
% Add /login, associate user name with PID (Check if name exists), prevent unlogged users from seeing chat.
% usernames can't start with # !!!
% Add username and timestamp to posts.
% Add chatrooms, make sure user only gets messages from rooms they are in.
% Add message user.

-module(ramen).
-author('Caz').

-export([start/1, keepstate/1, sendloop/1, reqhandler/0]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
	register(st, spawn(ramen, keepstate, [[]])),
	register(handle, spawn(ramen, reqhandler, [])),
	listen(Port).

listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(LSocket).

accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			addCon(Socket),
			accept(LSocket);
		{error, Reason} ->
			accept(LSocket)
	end.

%merge recv loop with sendloop, so they share pid
% or make a parent process, that way we can still do a timeout on the recv loop

addCon(Socket) ->
	PID = spawn(ramen, sendloop, [Socket]),
	spawn(fun() -> recvLoop(Socket, PID) end),
	st ! {add, PID}.

recvLoop(Socket, P) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			handle ! {P, Data},
			%st ! {broadcast, Data},
			io:format("Message from ~w: ~s~n", [P, Data]),
			recvLoop(Socket, P);
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

bcast(M, []) -> 
	io:format("Sent: ~s~n", [M]);
bcast(M, State) ->
	[{P,_} | Rest] = State,
	io:format("Sending to ~w~n", [P]),
	P ! {send, M},
	bcast(M, Rest).

cull(P, State) ->
	R = {P,anon},
	lists:delete(R, State).

keepstate(State) ->
	receive
		{add, P} ->
			NewState = [{P, anon} | State],
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

reqhandler() ->
	receive
		{P, M} ->
			case sanity:checkInput(M) of
				{ok, login, User} ->
					reqhandler();
				{ok, message, room, Room, Txt} ->
					st ! {broadcast, Txt},
					reqhandler();
				{ok, message, user, User, Txt} ->
					st ! {broadcast, Txt},
					reqhandler();
				{error, Reason} ->
					% Spit error Message back to user
					reqhandler()
			end
	end.
