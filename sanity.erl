% Sanity, checks input and parses meaning.

% Start ->
%	After proper recv(ie, no timeout),
%	Check for line end \r\n via CheckEnd,
%	Check for no additional line breaks,
%	Check for proper start code:
%		LOGIN , LOGOUT , MSG , JOIN , PART , (HELP , LIST)

% Notes:
% \r = 13, \n = 10

-module(sanity).
-author('Caz').
-export([checkEnd/1, checkBreaks/1, checkStart/1, checkStart/2, checkAndPass/1]).

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
	checkStart(S,["LOGIN ", "LOGOUT ", "MSG ", "JOIN ", "PART ", "HELP ", "LIST "]).

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
				{E} ->
					{error, lists:append([E, " is not a command"])}
			end;
		{error, Msg} ->
			{error, Msg}
	end.

checkAndPass(S) ->
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

checkLogin(Uname) ->
	HasSp = fun(X) -> if X == 32 -> true; true -> false end end,
	case lists:any(HasSp, Uname) of
		true ->
			{error, "No spaces allowed in usernames"};
		false ->
			case lists:nth(1, Uname) of
				35 ->
					{error, "Usernames may not start with #"};
				_ ->
					{ok, "Checking availability", Uname}
			end
	end.
