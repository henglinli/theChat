-module(utils).
-compile(export_all).

shadow_password(Password)->
    %bcrypt:hashpw(Password, bcrypt:gen_salt()).
    crypto:hash(sha256, Password).

check_password(Password, PasswordAttempt) ->
    Password =:= crypto:hash(sha256, PasswordAttempt).

require_login(SessionID) ->
    case boss_session:get_session_data(SessionID, user_id) of
	UserID ->
	    case boss_db:find(UserID) of
		{error, _} ->
		    {redirect, "/user/login"};
		User ->
		    {ok , User}
	    end;
	{error, _} ->
	    {redirect, "/user/login"}
    end.

remote_login(Name, Password) ->
    case boss_db:find_first(yuza, [{name, 'equals', Name}]) of
	undefined ->
	    {error, "Bad name"};
	User ->
	    case check_password(User:password(), Password) of
		false ->
		    {error, "Bad password"};
		true ->
		    ok
	    end
    end.

-spec syn_channel(atom(), string()) -> string().
syn_channel(date, Name) ->
    "DATE" ++ Name;

syn_channel(_, _) ->
    "undefined".

-spec ack_channel(atom(), string()) -> string().
ack_channel(date, Name) ->
    "ETAD" ++ Name;

ack_channel(_, _) ->
    "undefined".

-spec account_types() -> [string()].
account_types() ->
    ["sina", "tencent", "douban", "renren"].

-spec make_nakama(term(), term()) -> boolean().
make_nakama(Me, You) ->     
    MyNewNakama = nakama:new(id, Me:id(), You:name()),
    case MyNewNakama:save() of
	{ok, _} ->
	    NewNakama = nakama:new(id, You:id(), Me:name()),
	    case NewNakama:save() of
		{ok, _} ->
		    true;
		{error, [ErrorMessages]} ->
		    lager:error("boss_db save error: ~p", [ErrorMessages]),
		    false
	    end;
	{error, [ErrorMessages]} ->
	    lager:error("boss_db save error: ~p", [ErrorMessages]),
	    false
    end.
