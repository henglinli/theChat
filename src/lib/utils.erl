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
    "DATE" + Name;

syn_channel(_, _) ->
    "undefined".

-spec ack_channel(atom(), string()) -> string().
ack_channel(date, Name) ->
    "ETAD" + Name;

ack_channel(_, _) ->
    "undefined".

-spec account_types() -> [string()].
account_types() ->
    ["sina", "tencent", "douban", "renren"].
