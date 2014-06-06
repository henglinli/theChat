-module(yuza, [Id, 
	       Email,
	       Name,
	       RegisterTime,
	       Password,
	       Salt]).
%
-compile(export_all).
%
-has({comments, many}).
%
-has({owned_accounts, many}).
%
-has({nakamas, many}).

before_delete() ->
    case delete_nakama() of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    case delete_owned_accounts() of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    case delete_comments() of
			{error, Reason} ->
			    {error, Reason};
			ok ->
			    ok
		    end
	    end
    end.
		      
delete_nakama() ->
    case lists:all(fun(Nakama) ->
			   case boss_db:delete(Nakama:id()) of
			       ok -> 
				   true;
			       {error, Reason} ->
				   error_logger:info_msg(Reason),
				   false
			   end
		   end,
		   nakamas()) of
	false ->
	    {error, "delete nakama error"};
	true ->
	    ok
    end.

delete_owned_accounts() ->
    case lists:all(fun(Account) ->
			   case boss_db:delete(Account:id()) of
			       ok -> 
				   true;
			       {error, Reason} ->
				   error_logger:info_msg(Reason),
				   false
			   end
		   end,
		   owned_accounts()) of
	false ->
	    {error, "delete nakamas error"};
	true ->
	    ok
    end.

delete_comments() ->
    case lists:all(fun(Comment) ->
			   case boss_db:delete(Comment:id()) of
			       ok ->
				   true;
			       {error, Reason} ->
				   error_logger:info_msg(Reason),
				   false
			   end
		   end,
		   comments()) of
	false ->
	    {error, "delete comments error"};
	true ->
	    ok
    end.
