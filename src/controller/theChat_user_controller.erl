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
			    case pw_auth:check_password(erlang:list_to_binary(Password), User:password(), User:salt()) of
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
			    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of
				undefined ->
				    {pbkdf2, HexPass, Salt} = pw_auth:hash_password(erlang:list_to_binary(Password)),
				    User = yuza:new(id, Email, Name, boss_mq:now(""), HexPass, Salt),
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
