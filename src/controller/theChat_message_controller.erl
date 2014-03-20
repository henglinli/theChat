-module(theChat_message_controller, [Req, SessionID]).
-compile(export_all).
-include("protocol.hrl").

before_(_) ->
    user_lib:require_login(SessionID).

push('POST', [], User) ->
    case Req:post_param("who") of
	undefined ->
	    {json, [{error, "Need who"}]};
	Who ->
	    case Req:post_param("message", "hello") of
		undefined ->
		    {json, [{error, "fatal"}]};
		Message ->
		    Msg = message:new(
			    id, Who, Message, boss_mq:now(User:name())),
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
		    {size, Time},
		    {messages, Messages}]}
    end;

poll(_, [], User) ->
    {json, [{error, "Not support"}]}.

hello(_, [], User) ->
    {json, [{error, "hello"}]}.

syn('POST', [What], User) ->
    case Req:post_param("who") of
	undefined ->
	    {json, [{error, "Need who"}]};
	Who ->
	    case What of
		"make_friend" ->
		    Msg = message:new(
			    id, Who, ?make_friend_syn,
			    boss_mq:now("")),
		    case boss_mq:push(?make_friend_channel ++ Who, Msg) of
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
	"make_friend" ->
	    case boss_mq:poll(?make_friend_channel ++ User:name()) of
		{error, Reason} ->
		    {json, [{error, Reason}]};
		{ok, Time, Messages} ->
		    Results = lists:map(fun(Message) ->
						[{who, Message:from()},
						   {time, Message:time()}]
					end, Messages),
		    {json, [{error, "OK"},
			    {make_friend, Results}
			   ]}
	    end;
	_ ->
	    {json, [{error, "Not implemented"}]}
    end;

syn(_, [What], User) ->
    {json, [{error, "Not supported"}]}.

ack('POST', [What], User) ->
    case What of
	"make_friend" ->
	    case boss_mq:poll(?make_friend_channel) of
		{error, Reason} ->
		    {json, [{error, Reason}]};
		{ok, Time, Messages} ->
		    {json, [{error, "OK"}]}
	    end;
	_ ->
	    {json, [{error, "Not implemented"}]}
    end;

ack('GET', [What], User) ->
    {json, [{error, "OK"}]};

ack(_, [What], User) ->
    {json, [{error, "Not supported"}]}.
