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
	    case Req:post_param("date_type") of
		undefined ->
		    {json, [{error, "Need date type"}]};
		DateType ->
		    case Req:post_param("account_type") of
			undefined ->
			    {json, [{error, "Need account type"}]};
			AccountType ->
			    case lists:any(fun(TheType) ->
						   AccountType =:= TheType
					   end,
					   utils:account_types()) of
				false ->
				    {json, [{error, "Not supported type"}]};
				true ->
				    case Req:post_param("from") of
					undefined ->
					    {json, [{error, "Need from"}]};
					FromWho ->
					    case User:first_owned_account([{name, 'equals', FromWho},
									   {type, 'equals', AccountType}]) of
						{error, Reason} ->
						    {json, [{error, Reason}]};
						undefined ->
						    {json, [{error, "Bad from id"}]};
						_ ->
						    case Req:post_param("to") of
							undefined ->
							    {json, [{error, "Need to"}]};
							ToWho ->
							    case Yuza:first_owned_account([{name, 'equals', ToWho},
											   {type, 'equals', AccountType}]) of
								{error, Reason} ->
								    {json, [{error, Reason}]};
								undefined ->
								    {json, [{error, "Bad to id"}]};
								_ ->
								    UserId = User:id(),
								    case Yuza:first_comment([{from, 'equals', UserId},
											     {comment, 'equals', DateType},
											     {from_who, 'equals', FromWho},
											     {from_which, 'equals', AccountType},
											     {to_who, 'equals', ToWho}]) of
									undefined ->
									    NewComment = comment:new(id, Yuza:id(), UserId, DateType, FromWho, AccountType, ToWho, false),
									    case NewComment:save() of
										{error, [ErrorMessages]} ->
										    {json, [{error, ErrorMessages}]};
										{ok, _} ->
										    {json, [{error, "OK"}]}
									    end;
									Comment ->
									    {json, [{error, "Duplicated"},
										    {date, Comment:attributes()}
										   ]}
								    end
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
	    %% Dates = lists:map(fun(Comment) ->
	    %%			      [{type, Comment:from_which()},
	    %%			       {from, Comment:from_who()},
	    %%			       {to, Comment:to_who()}
	    %%			      ]
	    %%		      end,
	    %%		      Comments),
	    %% {json, [{error, "OK"},
	    %%	    {dates, Dates}
	    %%	   ]}
	    Dates = lists:map(fun(Comment) ->
				      Comment:attributes()
			      end,
			      Comments),
	    {json, [{error, "OK"},
		    {dates, Dates}]}
    end;

% to
%% date('DELETE', [To], User) ->
%%     case boss_db:find_first(yuza, [{name, 'equals', To}]) of
%%	{error, Reason} ->
%%	    {json, [{error, Reason}]};
%%	Yuza ->
%%	     case Req:param("type") of
%%		undefined ->
%%		    {json, [{error, "Need type"}]};
%%		Type ->
%%		    case lists:any(fun(TheType) ->
%%					   Type =:= TheType
%%				   end,
%%				   utils:account_types()) of
%%			false ->
%%			    {json, [{error, "Not supported type"}]};
%%			true ->
%%			    case Req:param("from") of
%%				undefined ->
%%				    {json, [{error, "Need from"}]};
%%				[] ->
%%				    {json, [{error, "Bad from id"}]};
%%				FromWho ->
%%				    case Req:param("to") of
%%					undefined ->
%%					    {json, [{error, "Need to"}]};
%%					[] ->
%%					    {json, [{error, "Bad to id"}]};
%%					ToWho ->
%%					    case Yuza:comments([{from_who, 'equals', FromWho}, {to_who, 'equals', ToWho}]) of
%%						{error, Reason} ->
%%						    {json, [{error, Reason}]};
%%						[] ->
%%						    {json, [{error, "Empty"}]};
%%						Comments ->
%%						    case lists:all(fun(Comment) ->
%%									   case boss_db:delete(Comment:id()) of
%%									       {error, Reason} ->
%%										   error_logger:info_msg(Reason),
%%										   false;
%%									       ok ->
%%										   true
%%									   end
%%								   end,
%%								   Comments) of
%%							false ->
%%							    {json, [{error, "delete comment error"}]};

%%							true ->
%%							    {json, [{error, "OK"}]}
%%						    end
%%					    end
%%				    end
%%			    end
%%		    end
%%	     end
%%     end;
date('DELETE', [CommentId], User) ->
    case boss_db:find_first(comment, [{from, 'equals', User:id()},
				      {id, 'equals', CommentId}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	_Comment ->
	    case boss_db:delete(CommentId) of
		{error, Reason} ->
		    lager:error("boss_db delete error: ~p", [Reason]),
		    {json, [{error, Reason}]};
		ok ->
		    {json, [{error, "OK"}]}
	    end
    end;
% to
date(_, _, _) ->
    {json, [{error, "Not supported"}]}.

% from
dated('GET', [], User) ->
    Comments = User:comments(),
    Dates = lists:map(fun(Comment) ->
			      Comment:attributes()
		      end,
		      Comments),
    {json, [{error, "OK"},
	    {dates, Dates}
	   ]};

% from
dated('DELETE', [CommentId], User) ->
    case User:first_comment([{id, 'equals', CommentId}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	_Comment ->
	    case boss_db:delete(CommentId) of
		{error, Reason} ->
		    lager:error("boss_db delete error: ~p", [Reason]),
		    {json, [{error, Reason}]};
		ok ->
		    {json, [{error, "OK"}]}
	    end
    end;

dated(_, _, _) ->
    {json, [{error, "Not supported"}]}.

new_dated('GET', [], User) ->
    Comments = User:comments(),
    Dates = lists:flatmap(fun(Comment) ->
			      case Comment:touched() of
				  true ->
				      [];
				  false ->
				      NewComment = Comment:set(touched, true),
				      case NewComment:save() of
					  {error, Reason} ->
					      lager:error("boss_db delete error: ~p", [Reason]),
					      [];
					  {ok, SavedNewComment} ->
					      SavedNewComment:attributes()
				      end
			      end
		      end,
		      Comments),
    {json, [{error, "OK"},
	    {dates, Dates}
	   ]};

new_dated(_, _, _) ->
    {json, [{error, "Not supported"}]}.
