-module(sockjs).

-export([close/1, close/3, info/1, send/2]).

-export([to_channel/2, to_session/1]).

%% Send data over a connection/channel.
-spec send(iodata(),
	   sockjs_session:conn() |
	   sockjs_multiplex_channel:channel()) -> ok.

send(Data, Conn = {sockjs_session, _}) ->
    sockjs_session:send(Data, Conn);
send(Data,
     Channel = {sockjs_multiplex_channel, _, _}) ->
    sockjs_multiplex_channel:send(Data, Channel).

%% Initiate a close of a connection/channel.
-spec close(sockjs_session:conn() |
	    sockjs_multiplex_channel:channel()) -> ok.

close(Conn) -> close(1000, "Normal closure", Conn).

-spec close(non_neg_integer(), string(),
	    sockjs_session:conn() |
	    sockjs_multiplex_channel:channel()) -> ok.

close(Code, Reason, Conn = {sockjs_session, _}) ->
    sockjs_session:close(Code, Reason, Conn);
close(Code, Reason,
      Channel = {sockjs_multiplex_channel, _, _}) ->
    sockjs_multiplex_channel:close(Code, Reason, Channel).

-spec info(sockjs_session:conn() |
	   sockjs_multiplex_channel:channel()) -> [{atom(),
						    any()}].

info(Conn = {sockjs_session, _}) ->
    sockjs_session:info(Conn);
info(Channel = {sockjs_multiplex_channel, _, _}) ->
    sockjs_multiplex_channel:info(Channel).

%% Get the backend connection of a channel.
-spec
     to_session(sockjs_multiplex_channel:channel()) -> sockjs_session:conn().

to_session({sockjs_multiplex_channel, Conn, _}) -> Conn.

%% Create a channel from a connection.
-spec to_channel(sockjs_session:conn(),
		 sockjs_multiplex_channel:topic()) -> sockjs_multiplex_channel:channel().

to_channel(Conn = {sockjs_session, _}, Topic) ->
    {sockjs_multiplex_channel, Conn, Topic}.
