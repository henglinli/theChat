-module(theChat_user_controller, [Req, SessionID]).
-compile(export_all).

login('GET', []) ->
    {json, [{error, "Please login"}]};

login('POST', []) ->
    Name = Req:post_param("name"),
    case boss_db:find(yuza, [{name, 'equals', Name}]) of
	[User] ->
	    case user_lib:check_password(User:password(),
					 Req:post_param("password")) of
		true ->
		    case boss_session:set_session_data(SessionID, 
						       user_id, User:id()) of
			ok ->
			    {json, [{error, "OK"}]};
			{error, Reason} ->
			    {json, [{error, Reason}]}
		    end;
		false ->
		    {json, [{error, "Bad password"}]}
	    end;
	[] ->
	    {json, [{error, "Bad name"}]}
    end;

login(_, []) ->
    {json, [{error, "Not supported"}]}.

register('POST', []) ->
    Email = Req:post_param("email"),
    Name = Req:post_param("name"),
    Password = user_lib:shadow_password(Req:post_param("password")),
    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of	
	undefined ->
	    User = yuza:new(
		     id, Email, Name, calendar:universal_time(), Password),
	    case User:save() of 
		{error, Errors} ->
		    {json, [{error, Errors}]};
		ok ->  
		    {json, [{error, "OK"}]}
	    end;	
	User ->
	    {json, [{error, "Conflict"}]}
    end;

register(_, []) ->
    {json, [{error, "Not supported"}]}.
