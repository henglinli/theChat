-module(theChat_message_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    utils:require_login(SessionID).

push('POST', [], User) ->
    case Req:param("who") of
	undefined ->
	    {json, [{error, "Need who"}]};
	Who ->
	    case Req:param("message", "hello") of
		undefined ->
		    {json, [{error, "fatal"}]};
		Message ->
		    From = User:name(),
		    Msg = message:new(From, Message, boss_mq:now(From)),
		    case boss_mq:push(Who, Msg) of
			undefined ->
			    {json, [{error, "fatal"}]};
			{ok, _} ->
			    {json, [{error, "OK"}]}
		    end
	    end
    end;

push(_, [], User) ->
    {json, [{error, "Not support"}]}.

poll('GET', [], User) ->
    case boss_mq:poll(User:name()) of
	{error, Reson} ->
	    {json, [{error, Reson}]};
	{ok, Time, Messages} ->
	    {json, [{error, "OK"},
		    {messages, Messages}]}
    end;

poll(_, [], User) ->
    {json, [{error, "Not support"}]}.

syn('POST', [What], User) ->
    case Req:param("who") of
	undefined ->
	    {json, [{error, "Need who"}]};
	Who ->
	    case What of
		"date" ->
		    case Req:param("type") of
			undefined ->
			    DateType = "moive";
			Type ->
			    DateType = Type
		    end,
		    Msg = message:new(
			    Who, DateType,
			    boss_mq:now("")),
		    Channel = utils:syn_channel(date, Who),
		    case boss_mq:push(Channel, Msg) of
			undefined ->
			    {json, [{error, "Fatal"}]};
			{ok, _} ->
			    {json, [{error, "OK"}]}
		    end;
		_ ->
		    {json, [{error, "Not implemented"}]}
	    end
    end;

syn('GET', [What], User) ->
    case What of
	"date" ->
	    Channel = utils:syn_channel(date, User:name()),
	    case boss_mq:poll(Channel) of
		{error, Reason} ->
		    {json, [{error, Reason}]};
		{ok, Time, Messages} ->
		    Results = lists:map(fun(Message) ->
						[{who, Message:from()},
						 {type, Message:message()},
						 {time, Message:time()}]
					end, Messages),
		    {json, [{error, "OK"},
			    {date_syn, Results}
			   ]}
	    end;
	_ ->
	    {json, [{error, "Not implemented"}]}
    end;

syn(_, [What], User) ->
    {json, [{error, "Not supported"}]}.

ack('POST', [What], User) ->
    case Req:param("who") of
	undefined ->
	    {json, [{error, "Need who"}]};
	Who ->
	    case What of
		"date" ->
		    case Req:param("type") of
			undefined ->
			    DateType = "moive";
			Type ->
			    DateType = Type
		    end,
		    Msg = message:new(
			    Who, DateType,
			    boss_mq:now("")),
		    Channel = utils:ack_channel(date, Who),
		    case boss_mq:push(Channel, Msg) of
			undefined ->
			    {json, [{error, "Fatal"}]};
			{ok, _} ->
			    {json, [{error, "OK"}]}
		    end;
		_ ->
		    {json, [{error, "Not implemented"}]}
	    end
    end;

ack('GET', [What], User) ->
    case What of
	"date" ->
	    Channel = utils:ack_channel(date, User:name()),
	    case boss_mq:poll(Channel) of
		{error, Reason} ->
		    {json, [{error, Reason}]};
		{ok, Time, Messages} ->
		    Results = lists:map(fun(Message) ->
						[{who, Message:from()},
						 {type, Message:message()},
						 {time, Message:time()}]
					end, Messages),
		    {json, [{error, "OK"},
			    {date_ack, Results}
			   ]}
	    end;
	_ ->
	    {json, [{error, "Not implemented"}]}
    end;

ack(_, [What], User) ->
    {json, [{error, "Not supported"}]}.
