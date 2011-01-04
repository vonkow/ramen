-module(rlist).
-author("Caz").

-export([roomList/1, roomState/1, roomSend/2]).

% This module handles all chat room related processes.
% roomList is the master list of all rooms currently in existance;
% it is queried when a user attempts to join a room, if the room exists
% the user is returned the room's process id and the room is given the user's id.
% If the room does not exist, the roomList creates a new room before doing the above.
% Each room is given its own roomState process; this tracks all users currently in
% the room and is used for broadcasting messages. If the last user in a room parts
% the roomState messages the roomList to remove itsself before terminating.

roomList(State) ->
	receive
		{getpid, P, Room} ->
			case getPid(Room, State) of
				{ok, Pid} ->
					P ! {ok, Pid},
					roomList(State);
				{error, add} ->
					NewPid = spawn(rlist, roomState, [[]]),
					P ! {ok, NewPid},
					roomList([{Room, NewPid} | State]);
				_ ->
					roomList(State)
			end;
		{remove, P} ->
			NewList = removeRoom(P, State),
			roomList(NewList)
	end.

getPid(_, []) ->
	{error, add};
getPid(Room, [Cur | Rest]) ->
	case Cur of
		{Room, Pid} ->
			{ok, Pid};
		_ ->
			getPid(Room, Rest)
	end.

removeRoom(P, State) ->
	removeRoom(P, State, []).

removeRoom(_, [], Acc) ->
	lists:reverse(Acc);
removeRoom(P, [Cur | Rest], Acc) ->
	case Cur of
		{_, P} ->
			lists:append(lists:reverse(Acc), Rest);
		_ ->
			removeRoom(P, Rest, [Cur | Acc])
	end.

roomState(State) ->
	receive
		{add, P} ->
			roomState([P | State]);
		{delete, P} ->
			case lists:delete(P, State) of
				[] ->
					roomlist ! {remove, self()};
				NewState ->
					roomState(NewState)
			end;
		{send, Msg} ->
			spawn(rlist, roomSend, [Msg, State]),
			roomState(State);
		_ ->
			roomState(State)
	end.

roomSend(_, []) ->
	ok;
roomSend(Msg, [Cur | Rest]) ->
	Cur ! {send, Msg},
	roomSend(Msg, Rest).
