-module(ulist).
-author("Caz").

-export([userList/1, getPid/3]).

userList(State)->
	receive
		{adduser, P, Name} ->
			case addUser(P, Name, State) of
				{ok, NewState} ->
					P ! {ok, login},
					userList(NewState);
				{error, Reason} ->
					P ! {error, Reason},
					userList(State)
			end;
		{remove, P} ->
			case removeUser(P, State) of
				{ok, NewState} ->
					P ! {ok, logout},
					userList(NewState);
				{error, Reason} ->
					P ! {error, Reason},
					userList(State)
			end;
		{getpid, Callback, Name} ->
			spawn(ulist, getPid, [Callback, Name, State]),
			userList(State);
		_ ->
			userList(State)
	end.

addUser(P, Name, State) ->
	addUser(P, Name, State, []).

addUser(P, Name, [], Acc) ->
	{ok, lists:reverse([{P, Name} | Acc])};
addUser(P, Name, [Cur | Rest], Acc) ->
	case Cur of
		{_, Name} ->
			{error, "USERNAME IN USE"};
		_ ->
			addUser(P, Name, Rest, [Cur | Acc])
	end.

removeUser(P, State) ->
	removeUser(P, State, []).

removeUser(_, [], _) ->
	{error, "NOT LOGGED IN"};
removeUser(P, [Cur | Rest], Acc) ->
	case Cur of
		{P, _} ->
			{ok, lists:append(lists:reverse(Acc), Rest)};
		_ ->
			removeUser(P, Rest, [Cur | Acc])
	end.

getPid(Callback, _, []) ->
	Callback ! {error, "USER NOT LOGGED IN"};
getPid(Callback, Name, [Cur | Rest]) ->
	case Cur of
		{P, Name} ->
			Callback ! {ok, P};
		_ ->
			getPid(Callback, Name, Rest)
	end.
