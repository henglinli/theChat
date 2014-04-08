-module(theChat_comment_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    utils:require_login(SessionID).

% to
date('POST', [To], User) ->
    case boss_db:find_first(yuza, [{name, 'equals', To}]) of
	{error, Reason} ->
	    {json, [{error, Reason}]};
	Yuza ->
	    case Req:post_param("type") of
		undefined ->
		    {json, [{error, "Need type"}]};
		Type ->
		    case lists:any(fun(TheType) ->
					   Type =:= TheType
				   end,
				   utils:account_types()) of
			false ->
			    {json, [{error, "Not supported type"}]};
			true ->
			    case Req:post_param("from") of
				undefined ->
				    {json, [{error, "Need from"}]};
				FromWho ->
				    case User:owned_accounts([{name, 'equals', FromWho}]) of
					{error, Reason} ->
					    {json, [{error, Reason}]};
					[] ->
					    {json, [{error, "Bad from id"}]};
					_ ->
					    case Req:post_param("to") of
						undefined ->
						    {json, [{error, "Need to"}]};
						ToWho ->
						    case Yuza:owned_accounts([{name, 'equals', ToWho}]) of
							{error, Reason} ->
							    {json, [{error, Reason}]};
							[] ->
							    {json, [{error, "Bad to id"}]};
							_ -> 
							    NewComment = comment:new(id, Yuza:id(), User:id(), "date", FromWho, Type, ToWho),
							    case NewComment:save() of
								{error, [ErrorMessages]} ->
								    {json, [{error, ErrorMessages}]};
								{ok, _} ->
								    {json, [{error, "OK"}]}
							    end
						    end
					    end
				    end
			    end
		    end
	    end
    end;
% to
date('GET', [], User) ->
    case boss_db:find(comment, [{from, 'equals', User:id()}]) of
	{error, Reason} ->
	    {json, [{error, Reason}]};
	Comments ->
	    Dates = lists:map(fun(Comment) ->
				      [{type, Comment:from_which()},
				       {from, Comment:from_who()},
				       {to, Comment:to_who()}
				      ]
			      end,
			      Comments),
	    {json, [{error, "OK"},
		    {dates, Dates}
			   ]}
    end;

% to
date('DELETE', [To], User) ->
    case boss_db:find_first(yuza, [{name, 'equals', To}]) of
	{error, Reason} ->
	    {json, [{error, Reason}]};
	Yuza ->
	     case Req:param("type") of
		undefined ->
		    {json, [{error, "Need type"}]};
		Type ->
		    case lists:any(fun(TheType) ->
					   Type =:= TheType
				   end,
				   utils:account_types()) of
			false ->
			    {json, [{error, "Not supported type"}]};
			true ->
			    case Req:param("from") of
				undefined ->
				    {json, [{error, "Need from"}]};
				[] ->
				    {json, [{error, "Bad from id"}]};
				FromWho ->
				    case Req:param("to") of
					undefined ->
					    {json, [{error, "Need to"}]};
					[] ->
					    {json, [{error, "Bad to id"}]};
					ToWho ->
					    case Yuza:comments([{from_who, 'equals', FromWho}, {to_who, 'equals', ToWho}]) of
						{error, Reason} ->
						    {json, [{error, Reason}]};
						[] ->
						    {json, [{error, "Empty"}]};
						Comments ->
						    case lists:all(fun(Comment) ->
									   case boss_db:delete(Comment:id()) of
									       {error, Reason} ->
										   error_logger:info_msg(Reason),
										   false;
									       ok ->
										   true
									   end
								   end,
								   Comments) of
							false ->
							    {json, [{error, "delete comment error"}]};
									       
							true ->
							    {json, [{error, "OK"}]}  
						    end
					    end
				    end
			    end
		    end
	     end
    end;
% to
date(_, _, _) ->
    {json, [{error, "Not supported"}]}.

% from
dated('GET', [], User) ->
    Comments = User:comments(),
    Dates = lists:map(fun(Comment) ->
			      [{type, Comment:from_which()},
			       {from, Comment:from_who()},
			       {to, Comment:to_who()}
			      ]
		      end,
		      Comments),
    {json, [{error, "OK"},
	    {dates, Dates}
	   ]};

% from
dated('DELETE', [From], User) ->
    case boss_db:find_first(yuza, [{name, 'equals', From}]) of
	{error, Reason} ->
	    {json, [{error, Reason}]};
	_ ->
	     case Req:param("type") of
		undefined ->
		    {json, [{error, "Need type"}]};
		Type ->
		    case lists:any(fun(TheType) ->
					   Type =:= TheType
				   end,
				   utils:account_types()) of
			false ->
			    {json, [{error, "Not supported type"}]};
			true ->
			    case Req:param("from") of
				undefined ->
				    {json, [{error, "Need from"}]};
				[] ->
				    {json, [{error, "Bad from id"}]};
				FromWho ->
				    case Req:param("to") of
					undefined ->
					    {json, [{error, "Need to"}]};
					[] ->
					    {json, [{error, "Bad to id"}]};
					ToWho ->
					    case User:comments([{from_who, 'equals', FromWho}, {to_who, 'equals', ToWho}]) of
						{error, Reason} ->
						    {json, [{error, Reason}]};
						Comments ->
						    case lists:all(fun(Comment) ->
									   case boss_db:delete(Comment:id()) of
									       {error, Reason} ->
										   error_logger:info_msg(Reason),
										   false;
									       ok ->
										   true
									   end
								   end,
								   Comments) of
							false ->
							    {json, [{error, "delete comment error"}]};
									       
							true ->
							    {json, [{error, "OK"}]}  
						    end
					    end
				    end
			    end
		    end
	     end
    end;

dated(_, _, _) ->
    {json, [{error, "Not supported"}]}.
