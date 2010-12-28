-module(rlist).
-author("Caz").

-export([roomList/1, roomState/1, roomSend/2]).

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
					roomList([{Room, NewPid} | State])
			end;
		{remove, P} ->
			roomList(removeRoom(P, State))
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
			roomState(State)
	end.

roomSend(_, []) ->
	ok;
roomSend(Msg, [Cur | Rest]) ->
	Cur ! {send, Msg},
	roomSend(Msg, Rest).
