-module(theChat_comment_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    utils:require_login(SessionID).

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
				   ["sina", "tencent", "douban", "renren"]) of
			false ->
			    {json, [{error, "Not supported type"}]};
			true ->
			    case Req:post_param("id") of
				undefined ->
				    {json, [{error, "Need type"}]};
				Id ->
				    NewComment = comment:new(id, Yuza:id(), Yuza:name(), "date", User:id(), Type, Id),
				    case NewComment:save() of
					{error, [ErrorMessages]} ->
					    {json, [{error, ErrorMessages}]};
					{ok, _} ->
					    {json, [{error, "OK"}]}
				    end
			    end
		    end
	    end
    end;

date('GET', [What], User) ->
    case What of
	"from" ->
	    Comments = User:comments(),
	    Dates = lists:map(fun(Comment) ->
				      [{type, Comment:from_which()},
				       {id, Comment:from_who()}
				      ]
			      end,
			      Comments),
	    {json, [{error, "OK"},
		    {dates, Dates}
		   ]};
	_ ->
	    case boss_db:find(comment, [{from_id, 'equals', User:id()}]) of
		{error, Reason} ->
		    {json, [{error, Reason}]};
		Comments ->
		    Dates = lists:map(fun(Comment) ->
					      [{type, Comment:from_which()},
					       {to, Comment:yuza_name()}
					      ]
				      end,
				      Comments),
		    {json, [{error, "OK"},
			    {dates, Dates}
			   ]}
		end
    end;

date('DELETE', [To], User) ->
    case boss_db:find_first(yuza, [{name, 'equals', To}]) of
	{error, Reason} ->
	    {json, [{error, Reason}]};
	Yuza ->
	    Comments = Yuza:comments(),
	    case lists:all(fun(Comment) ->
				   case Comment:from_id() =:= User:id() of
				       true ->
					   case boss_db:delete(Comment:id()) of
					       {error, _} ->
						   false;
					       ok ->
						   true
					   end;
				       false ->
					   true
				   end
			   end,
			   Comments) of
		false ->
		    {json, [{error, "Faltal"}]};
		true ->			    
		    {json, [{error, "OK"}]}
	    end
    end;

date(_, [To], User) ->
    {json, [{error, "Not supported"}]}.
