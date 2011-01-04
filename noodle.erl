-module(noodle).
-author("Caz").

-export([recvLoop/4, requester/2, userState/1, userState/3, sender/3]).

% This module contains the processes associated with each connected user.
% When a user connects, a receive loop and a User State process are created
% and associated with that particular connection.
% The User State is used to process requests, send the user messages and
% store the user's name and currently joined chat rooms.
% The Receive Loop is used to receive messages from the user and send them
% through the input checker "sanity". The Receive Loop is also responsible
% for making sure the user is not sending messages that are too long or too
% frequent. Repeated violations of these stipulations results in being kicked.

recvLoop(S, P, LastT, Strikes) ->
	case gen_tcp:recv(S, 0) of
		{ok, Data} when length(Data) < 1000 ->
			case timeCheck(LastT) of
				{ok, CurT} ->
					spawn(noodle, requester, [P, Data]),
					recvLoop(S, P, CurT, Strikes);
				{nok, CurT} ->
					if
						Strikes < 9 ->
							sendError(S, self(), "YOU ARE MESSAGING TOO QUICKLY"),
							recvLoop(S, P, CurT, Strikes+1);
						true ->
							sendError(S, self(), "YOU HAVE BEEN DISCONNECTED FOR REPEAT VIOLATIONS"),
							P ! seppuku
					end
			end;
		{ok, _} ->
			if
				Strikes < 9 ->
					sendError(S, self(), "MESSAGE IS TOO LONG, KEEP IT UNDER 1000 CHARACTERS"),
					recvLoop(S, P, LastT, Strikes+1);
				true ->
					sendError(S, self(), "YOU HAVE BEEN DISCONNECTED FOR REPEAT VIOLATIONS"),
					P ! seppuku
			end;
		{error, _} ->
			P ! seppuku
	end.

timeCheck({LastMegSec, LastSec}) ->
	{CurMegSec, CurSec, _} = erlang:now(),
	case CurMegSec of
		LastMegSec ->
			if 
				CurSec >= LastSec+1 ->
					{ok, {CurMegSec, CurSec}};
				true ->
					{nok, {CurMegSec, CurSec}}
			end;
		_ ->
			{ok, {CurMegSec, CurSec}}
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
			sendError(S, self(), "YOU ARE NOT LOGGED IN"),
			userState({S, Rx})
	end;
userState(S) ->
	receive
		{rx, Rx} ->
			userState({S, Rx});
		_ ->
			userState(S)
	end.

userState({S, Rx}, Uname, Rooms) ->
	receive
		{join, Room} ->
			HasR = fun({N, _}) -> if N == Room -> true; true -> false end end,
			case lists:any(HasR, Rooms) of
				true ->
					sendError(S, self(), "YOU HAVE ALREADY JOIND THIS ROOM"),
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
			sendError(S, self(), "YOU ARE ALREADY LOGGED IN"),
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
					gen_tcp:close(S),
					ok
			end
	end.

removeRoom(P, Room, Rooms) ->
	removeRoom(P, Room, Rooms, []).

removeRoom(_, _, [], _) ->
	{error, "YOU ARE NOT IN THIS ROOM"};
removeRoom(P, Room, [Cur | Rest], Acc) ->
	case Cur of
		{Room, RPid} ->
			RPid ! {delete, P},
			{ok, lists:append(lists:reverse(Acc), Rest)};
		_ ->
			removeRoom(P, Room, Rest, [Cur | Acc])
	end.

getRoomPid(_, []) ->
	{error, "YOU ARE NOT IN THIS ROOM"};
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
