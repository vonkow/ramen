-module(ulist).
-author("Caz").

-export([userList/1, getPid/3]).

userList(State)->
	receive
		{add, P, Name} ->
			case addUser(P, Name, State) of
				{ok, NewState} ->
					P ! {login, ok},
					userList(NewState);
				{error, Reason} ->
					P ! {login, error, Reason},
					userList(State)
			end;
		{remove, P} ->
			case removeUser(P, State) of
				{ok, NewState} ->
					P! {logout, ok},
					userList(NewState);
				{error, Reason} ->
					P ! {logout, error, Reason},
					userList(State)
			end;
		{getpid, P, Name} ->
			spawn(ulist, getPid, [P, Name, State]),
			userList(State)
	end.

addUser(P, Name, State) ->
	addUser(P, Name, State, []).

addUser(P, Name, [], Acc) ->
	lists:reverse([{P, Name} | Acc]);
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

getPid(Callback, Name, []) ->
	Callback ! {getpid, Name, error, "USER NOT LOGGED IN"};
getPid(Callback, Name, [Cur | Rest]) ->
	case Cur of
		{P, Name} ->
			Callback ! {getpid, Name, ok, P};
		_ ->
			getPid(Callback, Name, Rest)
	end.
