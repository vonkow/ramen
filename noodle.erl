-module(noodle).
-author("Caz").

-export([]).

% client connection
%	two-fold
%		sender
%			this is the core Pid of the client
%			this is the data-store of the client
%			used for sending messages to client
%		receiver
%			user for receiving and processing client messages
%			two states
%			unlogged
%				only accept LOGIN requests
%				spit back not logged in otherwise
%			logged
%				accept all requests except LOGIN
%				store username and joined groups
