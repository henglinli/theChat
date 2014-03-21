-module(theChat_main_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    utils:require_login(SessionID).

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
deregister('DELETE', [], User) ->
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
    %%	calendar:datetime_to_gregorian_seconds(Date1970),
    %% SecondsString = erlang:integer_to_list(Seconds),
    %% {json, [{error, "OK"},
    %%	    {id, User:id()},
    %%	    {email, User:email()},
    %%	    {name, User:name()},
    %%	    {register_time, SecondsString}
    %%	   ]};

profile('PUT', [], User) ->
    {json, [{error, "OK"}]};

profile(_, [], User) ->
    {json, [{error, "Not support"}]}.

%% create
account('POST', [], User) ->
    case Req:param("type") of
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
		    case Req:param("name") of
			undefined ->
			    {json, [{error, "Need type"}]};
			Name ->
			    case Req:param("access_token") of
				undefined ->
				    {json, [{error, "Need access token"}]};
				AccessToken ->
				    case Req:param("refresh_token") of
					undefined ->
					    {json, [{error, "Need refresh token"}]};
					RefreshToken ->
					    case Req:param("expires_in") of
						undefined ->
						    {json, [{error, "Need expires in"}]};
						ExpiresIn ->
						    Account = owned_account:new(
								id,
								User:id(),
								Type,
								Name,
								AccessToken,
								RefreshToken,
								ExpiresIn),
						    case Account:save() of
							{error, [ErrorMessages]} ->
							    {json, [{error, ErrorMessages}]};
							{ok, SavedAccount} ->
							    {json, [{error, "OK"},
								    {account, SavedAccount:attributes()}
								   ]}
						    end
					    end
				    end
			    end
		    end
	    end
    end;

%% update
account('PUT', [Id], User) ->
    case User:first_owned_account([{id, 'equals', Id}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	Account ->
	    case Req:param("type") of
		undefined ->
		    NewTypeAttr = {};
		Type ->
		    NewTypeAttr = {type, Type}
	    end,
	    case Req:param("name") of
		undefined ->
		    NewNameAttr = {};
		Name ->
		    NewNameAttr = {name, Name}
	    end,
	    case Req:param("refresh_token") of
		undefined ->
		    NewRefreshTokenAttr = {};		
		RefreshToken ->
		    NewRefreshTokenAttr = {refresh_token, RefreshToken}
	    end,
	    case Req:param("access_token") of
		undefined -> 
		    NewAccessTokenAttr = {};		
		AccessToken ->
		    NewAccessTokenAttr = {access_token, AccessToken}
	    end,
	    case Req:param("expires_in") of
		undefined ->
		    NewExpiresInAttr = {};		
		ExpiresIn ->
		    NewExpiresInAttr = {expires_in, ExpiresIn}
	    end,
	    NewAttrs = [NewTypeAttr, NewNameAttr, NewAccessTokenAttr, NewRefreshTokenAttr, NewExpiresInAttr],
	    NewAccount = Account:set(NewAttrs),
	    case NewAccount:save() of
		{error, [ErrorMessages]} ->
		    {json ,[{error, ErrorMessages}]};
		{ok, _} ->
		    {json, [{error, "OK"}]}
	    end
    end;
%% read
account('GET', [Id], User) ->
    case User:owned_accounts() of
	[] ->
	    {json, [{error, "Empty"}]};
	Accounts ->
	    case Id of
		"all" ->
		    Attributes = lists:map(fun(Account) ->
						   Account:attributes()
					   end,
					   Accounts),
		    {json, [{error, "Ok"},
			    {owned_accounts, Attributes}
			   ]};
		_ ->
		    case User:first_owned_account([{id, 'equals', Id}]) of
			undefined ->
			    {json, [{error, "Bad id"}]};
			Account ->
			    {json, [{error, "OK"},
				    {owned_account, Account:attributes()}
				   ]}
		    end
	    end
    end;

%% delete
account('DELETE', [Id], User) ->
    case User:owned_accounts() of
	[] ->
	    {json, [{error, "Empty"}]};
	Accounts ->
	    case Id of
		"all" ->
		    case lists:all(fun(Account) ->
					   case boss_db:delete(Account:id()) of
					       {error, _} ->
						   false;
					       ok ->
						   true
					   end
				   end,
				   Accounts) of
			false ->
			    {json, [{error, "Delete faltal"}]};
			true ->
			    {json, [{error, "OK"}]}
		    end;
		_ ->
		    case User:first_owned_account([{id, 'equals', Id}]) of
			undefined ->
			    {json, [{error, "Bad id"}]};
			Account ->
			    case boss_db:delete(Account:id()) of
				{error, Reason} ->
				    {json, [{error, Reason}]};
				ok ->
				    {json, [{error, "OK"}]}
			    end
		    end
	    end
    end;

%% not impl
account(_, [], User) ->
    {json, [{error, "Not supported"}]}.
