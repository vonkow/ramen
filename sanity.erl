% Sanity, checks input and parses meaning.

% Start ->
%	After proper recv(ie, no timeout),
%	Check for line end \r\n via CheckEnd,
%	Check for no additional line breaks,
%	Check for proper start code:
%		LOGIN , LOGOUT , MSG , JOIN , PART , (HELP , LIST)

% Notes:
% \r = 13, \n = 10, ' ' = 32, # = 35

-module(sanity).
-author('Caz').
-export([checkInput/1]).

checkEnd(S) ->
	case lists:suffix("\r\n", S) of
		true ->
			{ok, lists:subtract(S, "\r\n")};
		false ->
			{error, "Improper line ending"}
	end.


checkBreaks(S) ->
	HasR = fun(X) -> if X == 13 -> true; true -> false end end,
	HasN = fun(X) -> if X == 10 -> true; true -> false end end,
	case lists:any(HasN, S) of
		true ->
			{error, "Line break in message"};
		false ->
			case lists:any(HasR, S) of
				true ->
					{error, "Line break in message"};
				false ->
					{ok, S}
			end
	end.

checkStart(S) ->
	checkStart(S,["LOGIN ", "LOGOUT", "MSG ", "JOIN ", "PART ", "HELP ", "LIST "]).

checkStart(S,[]) ->
	{error, lists:append([S, " is not a command"])};
checkStart(S,A) ->
	[Cur | Rest] = A,
	case lists:prefix(Cur, S) of
		true ->
			{ok,  lists:subtract(Cur, " "), lists:subtract(S, Cur)};
		false ->
			checkStart(S, Rest)
	end.

parseStart(S) ->
	case checkStart(S) of
		{ok, Command, Data} ->
			case {Command} of
				{"LOGIN"} ->
					checkLogin(Data);
				{"LOGOUT"} ->
					checkLogout(Data);
				{"JOIN"} ->
					checkJoin(Data);
				{"PART"} ->
					checkPart(Data);
				{"MSG"} ->
					checkMsg(Data);
				{E} ->
					{error, lists:append([E, " is not a command"])}
			end;
		{error, Msg} ->
			{error, Msg}
	end.

checkInput(S) ->
	case checkEnd(S) of
		{ok, NewS} ->
			case checkBreaks(NewS) of
				{ok, NewS} ->
					parseStart(NewS);
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

checkSpace(Txt) ->
	HasSp = fun(X) -> if X == 32 -> true; true -> false end end,
	lists:any(HasSp, Txt).

checkStartHash(Txt) ->
	case lists:nth(1, Txt) of
		35 ->
			true;
		_ ->
			false
	end.

checkLogin(Uname) ->
	case checkSpace(Uname) of
		true ->
			{error, "No spaces allowed in usernames"};
		false ->
			case checkStartHash(Uname) of
				true ->
					{error, "Usernames may not start with #"};
				false ->
					{ok, login, Uname}
			end
	end.

checkLogout(_) ->
	{ok, logout}.

checkJoin(Room) ->
	case checkSpace(Room) of
		true ->
			{error, "No spaces allowed in room names"};
		false ->
			case checkStartHash(Room) of
				true ->
					{ok, join, Room};
				false ->
					{error, "Room names must start with #"}
			end
	end.

checkPart(Room) ->
	case checkSpace(Room) of
		true ->
			{error, "No spaces allowed in room names"};
		false ->
			case checkStartHash(Room) of
				true ->
					{ok, part, Room};
				false ->
					{error, "Room names must start with #"}
			end
	end.

checkMsg(Msg) ->
	case checkMsgData(Msg) of
		{ok, To, Txt} ->
			case checkStartHash(Msg) of
				true ->
					{ok, message, room, To, Txt};
				false ->
					{ok, message, user, To, Txt}
			end;
		{error, Info, Data} ->
			{error, lists:append([Info, Data])}
	end.


checkMsgData(Msg) ->
	checkMsgData(Msg, []).

checkMsgData([], A) ->
	{error, "Malformed message", A};
checkMsgData(Msg, A) ->
	[Cur | Rest] = Msg,
	case Cur of
		32 ->
			case A of
				[] ->
					{error, "Destination user or room not specified", Rest};
				[35] ->
					{error, "Destination room not specified", Rest};
				_ ->
					{ok, A, Rest}
			end;
		_ ->
			checkMsgData(Rest, lists:append([A, [Cur]]))
	end.
