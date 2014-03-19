-module(theChat_message_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    user_lib:require_login(SessionID).

push('POST', [], User) ->
    case  Req:post_param("who") of
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
			
