% TODO
% Prevent user from sending messages too quickly, add timestamp to userPID in state, check timestamp during post.
% Add timeout and char length check to received msgs
% Add /login, associate user name with PID (Check if name exists), prevent unlogged users from seeing chat.
% Add username and timestamp to posts.
% Add chatrooms, make sure user only gets messages from rooms they are in.
% Add message user.

-module(ramen).
-author('Caz').

-export([start/1, keepstate/1, sendloop/1, reqProcessor/2, sendUserMsg/1]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
	register(st, spawn(ramen, keepstate, [[]])),
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
	PID = spawn(ramen, sendloop, [Socket]),
	spawn(fun() -> recvloop(Socket, PID) end),
	st ! {add, PID}.

% Need to add something here to check frequency of posts and kill user if over limit
recvloop(Socket, P) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			spawn(ramen, reqProcessor, [P, Data]),
			io:format("Message from ~w: ~s~n", [P, Data]),
			recvloop(Socket, P);
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

% this will need fixing to work with users once logged in
cull(P, State) ->
	R = {P,[],[]},
	lists:delete(R, State).

checkUnameAvail(User, []) ->
	true;
checkUnameAvail(User, [Cur|Rest]) ->
	case Cur of
		{_,User,_} ->
			false;
		{_,_,_} ->
			checkUnameAvail(User,Rest)
	end.

checkIfLoggedIn(P, []) ->
	true;
checkIfLoggedIn(P, [Cur|Rest]) ->
	case Cur of
		{P, [], []} ->
			false;
		{P, A, _} ->
			true;
		{_, _, _} ->
			checkIfLoggedIn(P, Rest)
	end.

loginUser(P, User, State) ->
	R = {P,[],[]},
	TempState = lists:delete(R, State),
	lists:append([{P,User,[]}], TempState).
	
% go thru userlist, find P, check if already logged in, check if name is in use, if not, make P user.
addUser(P, User, State) ->
	case checkUnameAvail(User, State) of
		false ->
			{error, "Username in use"};
		true ->
			case checkIfLoggedIn(P, State) of
				true ->
					{error, "Already logged in"};
				false ->
					NewState = loginUser(P, User, State),
					{ok, NewState}
			end
	end.

logoutUser(P, State) ->
	logoutUser(P, State, []).

logoutUser(P, [], Remains) ->
	ok;
logoutUser(P, [Cur|Rest], NewState) ->
	case Cur of
		{P, [], _} ->
			{error, "Not logged in"};
		{P, U, _} ->
			TempState = lists:append(Rest, NewState),
			{ok, lists:append(TempState, [{P,[],[]}])};
		{_,_,_} ->
			logoutUser(P, Rest, lists:append(NewState, [Cur]))
	end.
			

keepstate(State) ->
	receive
		{add, P} ->
			NewState = [{P, [], []} | State],
			%io:format("State: ~w~n", [NewState]),
			keepstate(NewState);
		{remove, P} ->
			NewState = cull(P, State),
			%io:format("Culled: ~w~n", [P]),
			keepstate(NewState);
		{login, P, User} ->
			case addUser(P, User, State) of
				{ok, NewState} ->
					sendOk(P),
					keepstate(NewState);
				{error, Reason} ->
					sendError(P, Reason),
					keepstate(State)
			end;
		{logout, P} ->
			case logoutUser(P, State) of
				{ok, NewState} ->
					sendOk(P),
					keepstate(NewState);
				{error, Reason} ->
					sendError(P, Reason),
					keepstate(State)
			end;
		{message, user, FromP, ToN, Msg} ->
			spawn(ramen, sendUserMsg, [{State, FromP, ToN, Msg}]),
			keepstate(State);
		{broadcast, M} ->
			io:format("Got State: ~w~n", [State]),
			bcast(M, State),
			keepstate(State);
		quit ->
			ok
	end.

sendOk(P) ->
	P ! {send, "OK\r\n"}.

sendError(P, Reason) ->
	P ! {send, ["ERROR ", Reason, "\r\n"]}.

getUserPid(_, []) ->
	{error, "User not logged in"};
getUserPid(Uname, [Cur|Rest]) ->
	case Cur of
		{P, Uname, _} ->
			{ok, P};
		{_,_,_} ->
			getUserPid(Uname, Rest)
	end.

getUserName(_, []) ->
	{error, "User not logged in"};
getUserName(P, [Cur|Rest]) ->
	case Cur of
		{P, Uname, _} ->
			{ok, Uname};
		{_,_,_} ->
			getUserName(P, Rest)
	end.

sendUserMsg({State, FromP, ToN, Msg}) ->
	case checkIfLoggedIn(FromP, State) of
		true ->
			case getUserPid(ToN, State) of
				{ok, ToP} ->
					case getUserName(FromP, State) of
						{ok, FromN} ->
							FullMsg = ["GOTUSERMSG ",FromN," ",Msg],
							ToP ! {send, FullMsg},
							sendOk(FromP);
						{error, Reason} ->
							sendError(FromP, Reason)
							%io:format("Error: ~s~n",[Reason])
					end;
				{error, Reason} ->
					sendError(FromP, Reason)
					%io:format("Error: ~s~n",[Reason])
			end;
		false ->
			sendError(FromP, "Not logged in")
	end.

bcast(M, []) -> 
	io:format("Sent: ~s~n", [M]);
bcast(M, State) ->
	[{P,_,_} | Rest] = State,
	io:format("Sending to ~w~n", [P]),
	P ! {send, M},
	bcast(M, Rest).

reqProcessor(P, M) ->
	case sanity:checkInput(M) of
		{ok, login, User} ->
			io:format("Attempting Login of ~s~n", [User]),
			st ! {login, P, User};
		{ok, logout} ->
			st ! {logout, P};
		{ok, message, room, Room, Txt} ->
			%rst ! {message, P, Room, Txt},
			ok;
		{ok, message, user, User, Txt} ->
			st ! {message, user, P, User, Txt};
		{error, Reason} ->
			% Spit error Message back to user
			sendError(P, Reason)
	end.
