% TODO
% Prevent user from sending messages too quickly, add timestamp to userPID in state, check timestamp during post. Add timeout and char length check to received msgs. Add username and timestamp to posts.
% Add Check for blank messages
% !!! Remove users from roomstate on logout and timeout
% Handle removing timed out users from rooms
% sendloop need to shutdown recvloop

-module(ramen).
-author('Caz').

-export([start/1, userstate/1, roomstate/1, sendloop/2, reqProcessor/2, sendUserMsg/1, userLookup/3, callUserLookup/2, messageRoom/5]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
	register(st, spawn(ramen, userstate, [[]])),
	register(rooms, spawn(ramen, roomstate, [[]])),
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

addCon(Socket) ->
	PID = spawn(ramen, sendloop, [Socket, []]),
	spawn(fun() -> recvloop(Socket, PID) end).

% Need to add something here to check frequency of posts and kill user if over limit
recvloop(Socket, P) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			spawn(ramen, reqProcessor, [P, Data]),
			io:format("Message from ~w: ~s~n", [P, Data]),
			recvloop(Socket, P);
		{error, closed} ->
			% Add logic to remove user and user's joined rooms
			ok
	end.

reqProcessor(P, M) ->
	case sanity:checkInput(M) of
		{ok, login, User} ->
			st ! {login, P, User};
		{ok, logout} ->
			P ! logout,
			st ! {logout, P};
		{ok, join, Room} ->
			spawn(ramen, callUserLookup, [P, {join, P, Room}]);
		{ok, part, Room} ->
			spawn(ramen, callUserLookup, [P, {part, P, Room}]);
		{ok, message, room, Room, Txt} ->
			spawn(ramen, callUserLookup, [P, {message, P, Room, Txt}]);
		{ok, message, user, User, Txt} ->
			st ! {message, user, P, User, Txt};
		{error, Reason} ->
			sendError(P, Reason)
	end.

% This is the Pid that is referenced for connections, recvloop is given a copy of the pid.
sendloop(Socket, Rooms) ->
	receive
		{send, M} ->
			case gen_tcp:send(Socket, M) of
				ok ->
					io:format("Sent ~w to ~w~n", [ok, self()]),
					sendloop(Socket, Rooms);
				{error, _} ->
					io:format("Closed ~w~n", [self()]),
					% Here's where we need to add stuff that culls unreachable users from userstate and roomstate
					rooms ! {remove, self(), Rooms},
					%this needs rewriting
					%st ! {remove, self()},
					ok
			end;
		{addroom, Room} ->
			NewRooms = lists:append(Rooms, [Room]),
			io:format("slp Rooms: ~s~n", [NewRooms]),
			sendloop(Socket, NewRooms);
		{leaveroom, Room} ->
			NewRooms = lists:delete(Room, Rooms),
			io:format("slp Rooms: ~s~n", [NewRooms]),
			sendloop(Socket, NewRooms);
		logout ->
			rooms ! {remove, self(), Rooms},
			sendloop(Socket, [])
	end.

sendOk(P) ->
	P ! {send, "OK\r\n"}.

sendError(P, Reason) ->
	P ! {send, ["ERROR ", Reason, "\r\n"]}.

sendUserMsg({State, FromP, ToN, Msg}) ->
	case checkIfLoggedIn(FromP, State) of
		true ->
			case getUserPid(ToN, State) of
				{ok, ToP} ->
					case getUserName(FromP, State) of
						{ok, FromN} ->
							ToP ! {send, ["GOTUSERMSG ",FromN," ",Msg]},
							sendOk(FromP);
						{error, Reason} ->
							sendError(FromP, Reason)
					end;
				{error, Reason} ->
					sendError(FromP, Reason)
			end;
		false ->
			sendError(FromP, "Not logged in")
	end.

%merge with check if logged in?
checkUnameAvail(User, []) ->
	true;
checkUnameAvail(User, [Cur|Rest]) ->
	case Cur of
		{_,User} ->
			false;
		_ ->
			checkUnameAvail(User,Rest)
	end.

checkIfLoggedIn(P, []) ->
	false;
checkIfLoggedIn(P, [Cur|Rest]) ->
	case Cur of
		{P, _} ->
			true;
		_ ->
			checkIfLoggedIn(P, Rest)
	end.

loginUser(P, User, State) ->
	case checkUnameAvail(User, State) of
		false ->
			{error, "Username in use"};
		true ->
			case checkIfLoggedIn(P, State) of
				true ->
					{error, "Already logged in"};
				false ->
					{ok, lists:append(State, [{P, User}])}
			end
	end.

logoutUser(P, State) ->
	logoutUser(P, State, []).

logoutUser(P, [], Remains) ->
	{error, "You are not logged in"};
logoutUser(P, [Cur|Rest], NewState) ->
	case Cur of
		{P, U} ->
			P ! logout,
			{ok, lists:append(Rest, NewState)};
		{_,_} ->
			logoutUser(P, Rest, lists:append(NewState, [Cur]))
	end.

getUserPid(_, []) ->
	{error, "User not logged in"};
getUserPid(Uname, [Cur|Rest]) ->
	case Cur of
		{P, Uname} ->
			{ok, P};
		{_,_} ->
			getUserPid(Uname, Rest)
	end.

getUserName(_, []) ->
	{error, "User not logged in"};
getUserName(P, [Cur|Rest]) ->
	case Cur of
		{P, Uname} ->
			{ok, Uname};
		{_,_} ->
			getUserName(P, Rest)
	end.

userLookup(State, P, Callback) ->
	case getUserName(P, State) of
		{ok, Uname} ->
			Callback ! {ok, Uname};
		{error, _} ->
			Callback ! {error}
	end.

userstate(State) ->
	receive
		%blocks
		{login, P, User} ->
			case loginUser(P, User, State) of
				{ok, NewState} ->
					sendOk(P),
					userstate(NewState);
				{error, Reason} ->
					sendError(P, Reason),
					userstate(State)
			end;
		%blocks
		{logout, P} ->
			case logoutUser(P, State) of
				{ok, NewState} ->
					sendOk(P),
					userstate(NewState);
				{error, Reason} ->
					sendError(P, Reason),
					userstate(State)
			end;
		%non-blocking
		{message, user, FromP, ToN, Msg} ->
			spawn(ramen, sendUserMsg, [{State, FromP, ToN, Msg}]),
			userstate(State);
		%non-blocking
		{userlookup, P, Callback} ->
			spawn(ramen, userLookup, [State, P, Callback]),
			userstate(State);
		quit ->
			ok
	end.

joinRoom(State, P, Room) ->
	joinRoom(State, P, Room, []).

joinRoom([], P, Room, _) ->
	{create};
joinRoom([Cur|Rest], P, Room, Acc) ->
	case Cur of
		{Room,Users} ->
			HasP = fun(X) -> if X == P -> true; true -> false end end,
			case lists:any(HasP, Users) of
				false ->
					NewRoom = {Room,lists:append(Users,[P])},
					{ok, lists:append([Acc, [NewRoom], Rest])};
				true ->
					{error,"Already in room"}
			end;
		{_,_} ->
			joinRoom(Rest, P, Room, lists:append(Acc,[Cur]))
	end.

checkRoomLen(Users) when length(Users) > 1 ->
	ok;
checkRoomLen(_) ->
	nok.

partRoom(State, P, Room) ->
	partRoom(State, P, Room, []).

partRoom([], _, _, _) ->
	{error, "Not Logged in to room"};
partRoom([Cur|Rest], P, Room, Acc) ->
	case Cur of
		{Room, Users} ->
			HasP = fun(X) -> if X == P -> true; true -> false end end,
			case lists:any(HasP, Users) of
				true ->
					case checkRoomLen(Users) of
						ok ->
							NewRoom = {Room, lists:delete(P, Users)},
							{ok, lists:append([Acc, [NewRoom], Rest])};
						nok ->
							{ok, lists:append([Acc, Rest])}
					end;
				false ->
					{error, "Not Logged in to room"}
			end;
		{_,_} ->
			partRoom(Rest, P, Room, lists:append(Acc,[Cur]))
	end.

callUserLookup(P, PassOn) ->
	st ! {userlookup, P, self()},
	% maybe add a timeout on receive?
	receive
		{ok, Uname} ->
			rooms ! [Uname, PassOn];
		{error} ->
			sendError(P, "You are not logged in")
	end.

findRoom(_, []) ->
	{error, "No room of that name"};
findRoom(Room, [Cur|Rest]) ->
	case Cur of
		{Room, Userlist} ->
			{ok, Userlist};
		_ ->
			findRoom(Room, Rest)
	end.

broadcastMessage([], _, _) ->
	% message sent
	ok;
broadcastMessage([Cur|Rest], P, Msg) ->
	case Cur of
		P ->
			sendOk(P),
			broadcastMessage(Rest, P, Msg);
		_ ->
			Cur ! {send, Msg},
			broadcastMessage(Rest, P, Msg)
	end.

broadcastMessage(ToList, FromRoom, FromP, FromName, Txt) ->
	broadcastMessage(ToList, FromP, ["GOTROOMMSG ", FromName, " ", FromRoom, " ", Txt]).

messageRoom(State, P, Uname, Room, Txt) ->
	case findRoom(Room, State) of
		{ok, Users} ->
			HasP = fun(X) -> if X == P -> true; true -> false end end,
			case lists:any(HasP, Users) of
				true ->
					broadcastMessage(Users, Room, P, Uname, Txt);
				false ->
					sendError(P, "Not in room")
			end;
		{error, Reason} ->
			sendError(P, "No room of that name")
	end.

removeFromRooms(Rooms, P, Roomlist) ->
	removeFromRooms(Rooms, P, Roomlist, []).

removeFromRooms([], _, _, Acc) ->
	{ok, Acc};
removeFromRooms([{Room,Users}|Rest], P, Roomlist, Acc) ->
	HasRoom = fun(X) -> if X == Room -> true; true -> false end end,
	case lists:any(HasRoom, Roomlist) of
		true when length(Users) > 1 ->
			removeFromRooms(Rest, P, Roomlist, lists:append(Acc, [{Room, lists:delete(P, Users)}]));
		true ->
			removeFromRooms(Rest, P, Roomlist, Acc);
		false ->
			removeFromRooms(Rest, P, Roomlist, lists:append(Acc, [{Room, Users}]))
	end.

	% If current room is on roomlist, remove P from users
	% Add room to Acc after processing

roomstate(State) ->
	receive
		[Uname, {join, P, Room}] ->
			io:format("Trying ~w join ~s~n", [P, Room]),
			case joinRoom(State, P, Room) of
				{ok, NewState} ->
					P ! {addroom, Room},
					io:format("rst Rooms: ~w~n", [NewState]),
					sendOk(P),
					roomstate(NewState);
				{create} ->
					io:format("Trying to create room ~s for user ~w aka ~s~n",[Room, P, Uname]),
					io:format("Current State: ~w~n", [State]),
					TempState = [{Room,[P]} | State],
					io:format("rst Rooms: ~w~n", [TempState]),
					P ! {addroom, Room},
					sendOk(P),
					roomstate(TempState);
				{error, Reason} ->
					%send error
					sendError(P, Reason),
					roomstate(State)
			end;
		[Uname, {part, P, Room}] ->
			io:format("Trying ~w part ~s~n", [P, Room]),
			case partRoom(State, P, Room) of
				{ok, NewState} ->
					P ! {leaveroom, Room},
					io:format("rst Rooms: ~w~n", [NewState]),
					sendOk(P),
					roomstate(NewState);
				{error, Reason} ->
					sendError(P, Reason),
					roomstate(State)
			end;
		[Uname, {message, P, Room, Txt}] ->
			spawn(ramen, messageRoom, [State, P, Uname, Room, Txt]),
			roomstate(State);
		{remove, P, Rooms} ->
			case removeFromRooms(State, P, Rooms) of
				{ok, NewState} ->
					roomstate(NewState);
				{error, _} ->
					roomstate(State)
			end
	end.
