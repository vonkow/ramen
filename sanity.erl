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
			{error, "IMPROPER LINE ENDING"}
	end.


checkBreaks(S) ->
	HasR = fun(X) -> if X == 13 -> true; true -> false end end,
	HasN = fun(X) -> if X == 10 -> true; true -> false end end,
	case lists:any(HasN, S) of
		true ->
			{error, "ILLEGAL LINE BREAK IN MESSAGE"};
		false ->
			case lists:any(HasR, S) of
				true ->
					{error, "ILLEGAL LINE BREAK IN MESSAGE"};
				false ->
					{ok, S}
			end
	end.

checkStart(S) ->
	checkStart(S,["LOGIN ", "LOGOUT", "MSG ", "JOIN ", "PART ", "HELP ", "LIST "]).

checkStart(_,[]) ->
	{error, "NOT A PROPER COMMAND"};
checkStart(S,[Cur|Rest]) ->
	case lists:prefix(Cur, S) of
		true ->
			{ok,  lists:subtract(Cur, " "), lists:subtract(S, Cur)};
		false ->
			checkStart(S, Rest)
	end.

parseStart(S) ->
	case checkStart(S) of
		{ok, Command, Data} ->
			case Command of
				"LOGIN" ->
					checkLogin(Data);
				"LOGOUT" ->
					checkLogout(Data);
				"JOIN" ->
					checkJoin(Data);
				"PART" ->
					checkPart(Data);
				"MSG" ->
					checkMsg(Data);
				E ->
					{error, "NOT A PROPER COMMAND"}
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

checkLogin([]) ->
	{error, "NO USERNAME SPECIFIED"};
checkLogin(Uname) ->
	case checkSpace(Uname) of
		true ->
			{error, "NO SPACES ALLOWED IN USERNAME"};
		false ->
			case checkStartHash(Uname) of
				true ->
					{error, "USERNAME MAY NOT START WITH #"};
				false ->
					{ok, login, Uname}
			end
	end.

checkLogout(_) ->
	{ok, logout}.

checkJoin(Room) ->
	case checkSpace(Room) of
		true ->
			{error, "NO SPACES ALLOWED IN ROOMNAME"};
		false ->
			case checkStartHash(Room) of
				true ->
					{ok, join, Room};
				false ->
					{error, "ROOMNAME MUST START WITH #"}
			end
	end.

checkPart(Room) ->
	case checkSpace(Room) of
		true ->
			{error, "NO SPACES ALLOWED IN ROOMNAME"};
		false ->
			case checkStartHash(Room) of
				true ->
					{ok, part, Room};
				false ->
					{error, "ROOMNAME MUST START WITH #"}
			end
	end.

checkMsg(Msg) ->
	case checkMsgData(Msg) of
		{ok, To, Txt} ->
			case checkStartHash(Msg) of
				true ->
					{ok, message, room, To, [Txt,"\r\n"]};
				false ->
					{ok, message, user, To, [Txt,"\r\n"]}
			end;
		{error, Info, Data} ->
			{error, lists:append([Info, Data])}
	end.


checkMsgData(Msg) ->
	checkMsgData(Msg, []).

checkMsgData([], A) ->
	{error, "MALFORMED MESSAGE", A};
checkMsgData([Cur|Rest], A) ->
	case Cur of
		32 ->
			case A of
				[] ->
					{error, "DESTINATION USER OR ROOM NOT SPECIFIED", Rest};
				[35] ->
					{error, "DESTINATION ROOM NOT SPECIFIED", Rest};
				_ ->
					{ok, A, Rest}
			end;
		_ ->
			checkMsgData(Rest, lists:append([A, [Cur]]))
	end.
