-module(noodle).
-author("Caz").

-export([recvLoop/2, requester/2, userState/1, userState/3, sender/3]).

recvLoop(S, P) ->
	case gen_tcp:recv(S, 0) of
		{ok, Data} when length(Data) < 512 ->
			spawn(noodle, requester, [P, Data]),
			recvLoop(S, P);
		{ok, _} ->
			%send error, 512+ chars
			sendError(S, self(), "message over 512 chars"),
			recvLoop(S, P);
		{error, _} ->
			P ! seppuku
	end.

%re-write sanity to send correct msg, minus ok to userState
requester(P, Data) ->
	case sanity:checkInput(Data) of
		{ok, login, Uname} ->
			P ! {login, Uname};
		{ok, logout} ->
			P ! logout;
		{ok, join, Room} ->
			P ! {join, Room};
		{ok, part, Room} ->
			P ! {part, Room};
		{ok, message, room, Room, Msg} ->
			P ! {msg, room, Room, Msg};
		{ok, message, user, User, Msg} ->
			P ! {msg, user, User, Msg};
		{error, Reason} ->
			P ! {send, ["ERROR ",Reason, "\r\n"]}
	end.


userState({S, Rx}) ->
	receive
		{login, Uname} ->
			userlist ! {adduser, self(), Uname},
			receive
				{ok, login} ->
					sendOk(S, self()),
					userState({S, Rx}, Uname, []);
				{error, Reason} ->
					sendError(S, self(), Reason),
					userState({S, Rx})
			end;
		{send, Msg} ->
			spawn(noodle, sender, [S, self(), Msg]),
			userState({S, Rx});
		_ ->
			sendError(S, self(), "not logged in"),
			userState({S, Rx})
	end;
userState(S) ->
	receive
		{rx, Rx} ->
			userState({S, Rx})
	end.

userState({S, Rx}, Uname, Rooms) ->
	receive
		{join, Room} ->
			HasR = fun({N, _}) -> if N == Room -> true; true -> false end end,
			case lists:any(HasR, Rooms) of
				true ->
					sendError(S, self(), "already joined room"),
					userState({S, Rx}, Uname, Rooms);
				false ->
					roomlist ! {getpid, self(), Room},
					receive
						{ok, Pid} ->
							Pid ! {add, self()},
							sendOk(S, self()),
							userState({S, Rx}, Uname, [{Room, Pid} | Rooms])
					end
			end;
		{part, Room} ->
			case removeRoom(self(), Room, Rooms) of
				{ok, NewRooms} ->
					sendOk(S, self()),
					userState({S, Rx}, Uname, NewRooms);
				{error, Reason} ->
					sendError(S, self(), Reason),
					userState({S, Rx}, Uname, Rooms)
			end;
		{msg, room, Room, Msg} ->
			case getRoomPid(Room, Rooms) of
				{ok, RPid} ->
					RPid ! {send, ["GOTROOMMSG ", Uname, " ", Room, " ", Msg]},
					sendOk(S, self());
				{error, Reason} ->
					sendError(S, self(), Reason)
			end,
			userState({S, Rx}, Uname, Rooms);
		{msg, user, User, Msg} ->
			userlist ! {getpid, self(), User},
			receive
				{ok, UPid} ->
					UPid ! {send, ["GOTUSERMSG ", Uname, " ", Msg]},
					sendOk(S, self()),
					userState({S, Rx}, Uname, Rooms);
				{error, Reason} ->
					sendError(S, self(), Reason),
					userState({S, Rx}, Uname, Rooms)
			end;
		{send, Msg} ->
			spawn(noodle, sender, [S, self(), Msg]),
			userState({S, Rx}, Uname, Rooms);
		{error, Reason} ->
			sendError(S, self(), Reason),
			userState({S, Rx}, Uname, Rooms);
		{login, _} ->
			sendError(S, self(), "already logged in"),
			userState({S, Rx}, Uname, Rooms);
		logout ->
			removeFromRooms(self(), Rooms),
			userlist ! {remove, self()},
			receive
				{ok, logout} ->
					sendOk(S, self()),
					userState({S, Rx})
			end;
		seppuku ->
			removeFromRooms(self(), Rooms),
			userlist ! {remove, self()},
			receive
				{ok, logout} ->
					exit(Rx, kill),
					ok
			end
	end.

removeRoom(P, Room, Rooms) ->
	removeRoom(P, Room, Rooms, []).

removeRoom(_, _, [], _) ->
	{error, "Not in room"};
removeRoom(P, Room, [Cur | Rest], Acc) ->
	case Cur of
		{Room, RPid} ->
			RPid ! {delete, P},
			{ok, lists:append(lists:reverse(Acc), Rest)};
		_ ->
			removeRoom(P, Room, Rest, [Cur | Acc])
	end.

getRoomPid(_, []) ->
	{error, "Not in room"};
getRoomPid(Room, [Cur | Rest]) ->
	case Cur of
		{Room, RPid} ->
			{ok, RPid};
		_ ->
			getRoomPid(Room, Rest)
	end.

removeFromRooms(_, []) ->
	ok;
removeFromRooms(P, [{_, RPid} | Rest]) ->
	RPid ! {delete, P},
	removeFromRooms(P, Rest).

sender(S, P, Msg) ->
	case gen_tcp:send(S, Msg) of
		ok ->
			ok;
		{error, _} ->
			P ! seppuku
	end.

sendOk(S, P) ->
	spawn(noodle, sender, [S, P, "OK\r\n"]).

sendError(S, P, Reason) ->
	spawn(noodle, sender, [S, P, ["ERROR ", Reason, "\r\n"]]).
