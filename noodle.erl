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
			recvLoop(S, P);
		{error, _} ->
			P ! seppuku
	end.

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
			P ! {message, room, Room, Msg};
		{ok, message, user, User, Msg} ->
			P ! {message, user, User, Msg};
		{error, Reason} ->
			P ! {send, ["ERROR ",Reason]}
	end.

userState({S, Rx}) ->
	receive
		{login, Uname} ->
			userlist ! {add, self(), Uname,
			receive
				{ok, login} ->
					% send ok
					userState({S, Rx}, Uname, []);
				{error, Reason} ->
					%send error, uname in use
					userState({S, Rx})
			end;
		_ ->
			%send, error, not logged in
			userState({S, Rx})
	end.

userState({S, Rx}, Uname, Rooms) ->
	receive
		{join, Room} ->
			HasR = fun({N, P}) -> if N == Room -> true; true -> false end end,
			case lists:any(HasR, Rooms) of
				true ->
					%send error, already in room
					userState({S, Rx}, Uname, Rooms);
				false ->
					roomlist ! {getpid, self(), Room},
					receive
						{ok, Pid} ->
							Pid ! {add, self()},
							%send Ok
							userState({S, Rx}, Uname, [{Room, Pid} | Rooms])
					end
			end;
		{part, Room} ->
			case removeRoom(P, Room, Rooms) of
				{ok, NewRooms} ->
					%send ok
					userState({S, Rx}, Uname, NewRooms});
				{error, Reason} ->
					%send error, not in room
					userState({S, Rx}, Uname, Rooms)
			end;
		{msg, room, Room, Msg} ->
			case getRoomPid(Room, Rooms) of
				{ok, rPid} ->
					rPid ! {send, ["GOTROOMMSG ", Uname, " ", Room, " ", Msg]};
					%send ok
				{error, Reason} ->
					%send error, not in room
			end,
			userState({S, Rx}, Uname, Rooms);
		{msg, user, User, Msg} ->
			userlist ! {getpid, self(), User},
			receive
				{ok, uPid} ->
					uPid ! {send, ["GOTUSERMSG ", Uname, " ", Msg]};
					%send ok
				{error, Reason} ->
					%send error, user not logged in
			end,
			userState({S, Rx}, Uname, Rooms);
		{send, Msg} ->
			spawn(noodle, sender, [S, self(), Msg]),
			userState({S, Rx}, Uname, Rooms);
		{login, _} ->
			%send error, already logged in
			userState({S, Rx}, Uname, Rooms);
		logout ->
			removeFromRooms(self(), Rooms),
			userlist ! {remove, self()},
			receive
				{ok, logout} ->
					%send ok
					userState({S, Rx})
			end;
		seppuku ->
			removeFromRooms(self(), Rooms),
			userlist ! {remove, self()},
			receive
				{ok, logout} ->
					kill(Tx),
					kill(Rx),
					ok;
			end
	end.

removeRoom(P, Room, Rooms) ->
	removeRoom(P, Room, Rooms, []).

removeRoom(_, _, [], Acc) ->
	{error, "Not in room"};
removeRoom(P, Room, [Cur | Rest], Acc) ->
	case Cur of
		{Room, rPid} ->
			rPid ! {delete, P},
			{ok, lists:append(lists:reverse(Acc), Rest)};
		_ ->
			removeRoom(P, Room, Rest, [Cur | Acc])
	end.

getRoomRid(_, []) ->
	{error, "Not in room"};
getRoomPid(Room, [Cur | Rest]) ->
	case Cur of
		{Room, rPid} ->
			{ok, rPid};
		_ ->
			getRoomPid(Room, Rest)
	end.

removeFromRooms(_, []) ->
	ok;
removeFromRooms(P, [{_, rPid} | Rest]) ->
	rPid ! {delete, P},
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

%Logged userState commands:
%	{join, Room}
%		check Rooms for room
%			false ->
%				message roomList ! get rPid by Name
%				add uPid to roomState
%				add rPid to Rooms
%				send ok
%			true ->
%				send error, already in room
%		call self
%	{part, Room}
%		check Rooms for room
%			true ->
%				message roomState ! remove uPid
%				remove Room from Rooms
%				send ok
%			false ->
%				send error, not in room
%		call self
%	{msg, Room, Msg}
%		check Rooms for room
%			true ->
%				message room ! send message
%				send ok
%			false ->
%				error, not in room
%		call self
%	{msg, User, Msg}
%		message userlist ! get toPid
%			{ok, Pid} ->
%				message Pid ! send message
%				send ok
%			false ->
%				send error, user not logged in
%		call self
%	{send, Msg}
%		spawn sender to send Msg
%	logout
%		message userstate ! remove uPid
%		message all Rooms ! remove uPid
%		send ok
%		call self unlogged 
%	seppuku
%		do logout
%		kill(Recv)
%		kill(Send)
%		ok
