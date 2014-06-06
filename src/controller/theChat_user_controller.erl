-module(theChat_user_controller, [Req, SessionID]).
-compile(export_all).

login('POST', []) ->
    case Req:param("name") of
	undefined ->
	    {json, [{error, "Need name"}]};
	Name ->
	    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of
		undefined ->
		    {json, [{error, "Bad name"}]};
		User ->
		    case Req:param("password") of
			undefined ->
			    {json, [{error, "Need password"}]};
			Password ->
			    case utils:check_password(User:password(), Password) of
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
		    end
	    end
    end;

login(_, []) ->
    {json, [{error, "Please login"}]}.

register('POST', []) ->
    case Req:param("email") of 
	undefined -> 
	    {json, [{error, "Need email"}]};
	Email ->
	    case Req:param("name") of
		undefined ->
		    {json, [{error, "Need name"}]};
		Name ->
		    case Req:param("password") of
			undefined -> 
			    {json, [{error, "Need password"}]};
			Password ->			    
			    Shadow = utils:shadow_password(Password),
			    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of			
				undefined ->
				    User = yuza:new(
					     id, Email, Name, boss_mq:now(""), Shadow),				  
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
