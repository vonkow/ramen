-module(ramen).
-author('Caz').

-export([start/1, stop/0, listen/3, accept/1]).

start(Port) ->
	U = spawn(ulist, userList, [[]]),
	R = spawn(rlist, roomList, [[]]),
	register(userlist, U),
	register(roomlist, R),
	register(listener, spawn(ramen, listen, [Port, U, R])).

stop() ->
	listener ! {quit, self()},
	receive
		{S, U, R} ->
			exit(S, kill),
			exit(U, kill),
			exit(R, kill)
	end.

listen(Port, U, R) ->
	{ok, LSocket} = gen_tcp:listen(Port, [list, {packet, 0}, {active, false}, {reuseaddr, true}]),
	S = spawn(ramen, accept, [LSocket]),
	register(serv, S),
	receive
		{quit, P} ->
			P ! {S, U, R}
	end.

accept(LSocket) ->
	%Add timeout here?
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			Pid = spawn(noodle, sendLoop, [Socket]),
			spawn(noodle, recvLoop(Socket, Pid).
			accept(LSocket);
		{error, Reason} ->
			accept(LSocket)
	end.
