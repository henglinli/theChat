-module(theChat_user_controller, [Req, SessionID]).
-compile(export_all).

login('GET', []) ->
    {json, [{error, "Please login"}]};

login('POST', []) ->
    Name = Req:post_param("name"),
    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of
	undefined ->
	    {json, [{error, "Bad name"}]};
	User ->
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
	    end
    end;

login(_, []) ->
    {json, [{error, "Not supported"}]}.

register('POST', []) ->
    case Req:post_param("email") of 
	undefined -> 
	    {json, [{error, "Need email"}]};
	Email ->
	    case Req:post_param("name") of
		undefined ->
		    {json, [{error, "Need name"}]};
		Name ->
		    case Req:post_param("password") of
			undefined -> 
			    {json, [{error, "Need password"}]};
			Password ->			    
			    Shadow = user_lib:shadow_password(Password),
			    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of			
				undefined ->
				    User = yuza:new(
					     id, Email, Name, calendar:universal_time(), Shadow),				  
				    case User:save() of
					{error, [ErrorMessages]} ->
					    {json, [{error, ErrorMessages}]};
					{ok, _} ->
					    {json, [{error, "OK"}]}
				    end;
				User ->
				    {json, [{error, "Conflict"}]}     
			    end
		    end				
	    end
    end;
			
register(_, []) ->
    {json, [{error, "Not supported"}]}.
