-module(ulist).
-author("Caz").

-export([userList/1, getPid/3]).

% This module stores a master list of all logged in users and their associated
% process Ids. It is queried every time a user logs in or out or sends a message
% to another user, hence it has the potential to be a bottleneck. Due to this fact,
% I attempted to move as much of the verification and processing to the User State
% found in the module "noodle". If this chat server was to be used in a production
% environment, I might consider creating multiple user lists based upon the starting
% letter of the user's name or something like that. For now however, I think it can 
% hold up the the abuse it will receive.

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
