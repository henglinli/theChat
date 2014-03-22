-module(owned_account, 
	[Id,
	 YuzaId,
	 Type,
	 Name,
	 AccessToken,
	 RefreshToken,
	 ExpiresIn
	]).

-compile(export_all).

-belongs_to(yuza).

-has({other_nakamas, many}).

before_delete() ->
    case delete_other_nakama() of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    ok
    end.
		      
delete_other_nakama() ->
    case lists:all(fun(Nakama) ->
			   case boss_db:delete(Nakama:id()) of
			       ok -> 
				   true;
			       {error, Reason} ->
				   error_logger:info_msg(Reason),
				   false
			   end
		   end,
		   other_nakamas()) of
	false ->
	    {error, "delete nakama error"};
	true ->
	    ok
    end.
