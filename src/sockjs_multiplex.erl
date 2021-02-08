-module(sockjs_multiplex).

-behaviour(sockjs_service).

-export([init_state/1, init_state/2]).

-export([sockjs_handle/3, sockjs_init/2,
	 sockjs_terminate/2]).

-record(service, {callback, state, vconn}).

-record(authen_callback, {callback, success = false}).

%% --------------------------------------------------------------------------

init_state(Services, {AuthenCallback, Options}) ->
    L = [{Topic,
	  #service{callback = Callback, state = State}}
	 || {Topic, Callback, State} <- Services],
    Extra = case lists:keyfind(state, 1, Options) of
	      {state, ExtraValue} ->
		  case erlang:is_list(ExtraValue) of
		    true -> ExtraValue;
		    false -> []
		  end;
	      false -> []
	    end,
    % Services, Channels, AuthenCallback, Extra
    {orddict:from_list(L), orddict:new(),
     #authen_callback{callback = AuthenCallback,
		      success = false},
     Extra}.

init_state(Services) ->
    init_state(Services, {undefined, []}).

%% Get result of authentication callback if it exists.
%% Otherwise return ``authen_callback_not_found``.
%% Authentication callback should return {ok, State} or {success, State}.
get_authen_callback_result(#authen_callback{callback =
						AuthenCallback},
			   Handle, What, Extra) ->
    case erlang:is_function(AuthenCallback) of
      true -> AuthenCallback(Handle, What, Extra);
      false -> authen_callback_not_found
    end.

sockjs_init(Conn,
	    {Services, Channels, AuthenCallbackRec, Extra} = S) ->
    case get_authen_callback_result(AuthenCallbackRec, Conn,
				    init, Extra)
	of
      authen_callback_not_found -> {ok, S};
      {ok, Extra1} ->
	  {ok, {Services, Channels, AuthenCallbackRec, Extra1}}
    end.

sockjs_handle_via_channel(Conn, Data,
			  {Services, Channels, AuthenCallbackRec, Extra}) ->
    [Type, Topic, Payload] = split($,, binary_to_list(Data),
				   3),
    case orddict:find(Topic, Services) of
      {ok, Service} ->
	  Channels1 = action(Conn, {Type, Topic, Payload},
			     Service, Channels, Extra),
	  {ok, {Services, Channels1, AuthenCallbackRec, Extra}};
      _Else ->
	  {ok, {Services, Channels, AuthenCallbackRec, Extra}}
    end.

sockjs_handle(Conn, Data,
	      {Services, Channels,
	       #authen_callback{success = Success} = AuthenCallbackRec,
	       Extra} =
		  S) ->
    case Success of
      true -> sockjs_handle_via_channel(Conn, Data, S);
      false ->
	  case get_authen_callback_result(AuthenCallbackRec, Conn,
					  {recv, Data}, Extra)
	      of
	    authen_callback_not_found ->
		sockjs_handle_via_channel(Conn, Data,
					  {Services, Channels,
					   AuthenCallbackRec, Extra});
	    {success, Extra1} ->
		{ok,
		 {Services, Channels,
		  AuthenCallbackRec#authen_callback{success = true},
		  Extra1}};
	    {ok, Extra1} ->
		{ok, {Services, Channels, AuthenCallbackRec, Extra1}}
	  end
    end.

sockjs_terminate(Conn,
		 {Services, Channels, AuthenCallbackRec, Extra}) ->
    case get_authen_callback_result(AuthenCallbackRec, Conn,
				    closed, Extra)
	of
      {ok, Extra1} -> ok;
      _Else -> Extra1 = Extra
    end,
    _ = [{emit(closed, Channel)}
	 || {_Topic, Channel} <- orddict:to_list(Channels)],
    {ok,
     {Services, orddict:new(), AuthenCallbackRec, Extra1}}.

action(Conn, {Type, Topic, Payload}, Service, Channels,
       Extra) ->
    case {Type, orddict:is_key(Topic, Channels)} of
      {"sub", false} ->
	  Channel = Service#service{state =
					Service#service.state ++ Extra,
				    vconn =
					{sockjs_multiplex_channel, Conn,
					 Topic}},
	  orddict:store(Topic, emit(init, Channel), Channels);
      {"uns", true} ->
	  Channel = orddict:fetch(Topic, Channels),
	  emit(closed, Channel),
	  orddict:erase(Topic, Channels);
      {"msg", true} ->
	  Channel = orddict:fetch(Topic, Channels),
	  orddict:store(Topic, emit({recv, Payload}, Channel),
			Channels);
      _Else ->
	  %% Ignore
	  Channels
    end.

emit(What,
     Channel = #service{callback = Callback, state = State,
			vconn = VConn}) ->
    case Callback(VConn, What, State) of
      {ok, State1} -> Channel#service{state = State1};
      ok -> Channel
    end.

%% --------------------------------------------------------------------------

split(Char, Str, Limit) when Limit > 0 ->
    Acc = split(Char, Str, Limit, []), lists:reverse(Acc);
split(_Char, Str, 0) -> [Str].

split(_Char, Str, 1, Acc) -> [Str | Acc];
split(Char, Str, Limit, Acc) ->
    {L, R} = case string:chr(Str, Char) of
	       0 -> {Str, ""};
	       I ->
		   {string:substr(Str, 1, I - 1),
		    string:substr(Str, I + 1)}
	     end,
    split(Char, R, Limit - 1, [L | Acc]).
