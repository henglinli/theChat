-module(theChat_main_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    user_lib:require_login(SessionID).

logout('POST', [], User) ->
    case boss_session:delete_session(SessionID) of
	ok ->
	    {json, [{error, "OK"}]};
	{error, Reason} ->
	    {json, [{error, Reason}]}
    end;

logout(_, [], User) ->
    {json, [{error, "Not supported"}]}.

% delete user
deregister('POST', [], User) ->
    case boss_db:delete(User:id()) of
	ok ->
	    {json, [{error, "OK"}]};
	{error, Reason} ->
	    {json, [{error, Reason}]}
    end;

deregister(_, [], User) ->
    {json, [{error, "Not supported"}]}.

profile('GET', [], User) ->
    UserAttributes = User:attributes(),
    UserInfo = lists:sublist(UserAttributes, 4),
    {json, [{error, "OK"},
	    {profile, UserInfo}
	    ]};
    %% RegisterTime = User:register_time(),
    %% Date1970 = {{1970, 1, 1}, {0, 0, 0}},
    %% Seconds = calendar:datetime_to_gregorian_seconds(RegisterTime) - 
    %% 	calendar:datetime_to_gregorian_seconds(Date1970),
    %% SecondsString = erlang:integer_to_list(Seconds),
    %% {json, [{error, "OK"},
    %% 	    {id, User:id()},
    %% 	    {email, User:email()},
    %% 	    {name, User:name()},
    %% 	    {register_time, SecondsString}
    %% 	   ]};

profile(_, [], User) ->
    {json, [{error, "Not support"}]}.
