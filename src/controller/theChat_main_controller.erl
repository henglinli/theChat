-module(theChat_main_controller, [Req, SessionID]).
-compile(export_all).

before_(_) ->
    utils:require_login(SessionID).

logout('POST', [], _) ->
    case boss_session:delete_session(SessionID) of
	ok ->
	    {json, [{error, "OK"}]};
	{error, Reason} ->
	    {json, [{error, Reason}]}
    end;

logout(_, [], _) ->
    {json, [{error, "Not supported"}]}.

% delete user
deregister('DELETE', [], User) ->
    case boss_db:delete(User:id()) of
	ok ->
	    {json, [{error, "OK"}]};
	{error, Reason} ->
	    {json, [{error, Reason}]}
    end;

deregister(_, [], _) ->
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
    case Req:param("password") of
	undefined ->
	    {json, [{error, "Need password"}]};
	Password ->
	    Shadow = utils:shadow_password(Password),
	    NewUser = User:set(password, Shadow),
	    case NewUser:save() of
		{error, [ErrorMessages]} ->
		    {json, [{error, ErrorMessages}]};
		{ok, _}  ->
		    case boss_session:delete_session(SessionID) of
			{error, Reason} ->
			    {json, [{error, Reason}]};
			ok ->
			    {json, [{error, "OK"}]}
		    end
	    end
    end;

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
account(_, [], _) ->
    {json, [{error, "Not supported"}]}.

update('POST', [Id], User) ->
    %% {json, [{error, Req:request_body()}]};
    case User:first_owned_account([{id, 'equals', Id}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	_ ->
	    case jsx:decode(Req:request_body()) of
		{incomplete, _} ->
		    {json, [{error, "Bad request body"}]};
		[] ->
		    {json, [{error, "Bad request body"}]};
		Json ->
		    %% find each name in other_nakama
		    lists:foreach(fun(Name) ->
					  case boss_db:find(other_nakama, [{name, 'equals', erlang:binary_to_list(Name)}]) of
					      {error, Reason} ->
						  error_logger:info_msg(Reason);
					      OtherNakamas ->
						  lists:foreach(fun(OtherNakama) ->
									Dads = OtherNakama:belongs_to(),
									lists:foreach(fun(Dad) ->
											      {_, Account} = Dad,
											      Yuzas = Account:belongs_to(),
											      lists:foreach(fun(Yuza) ->
														    NewNakama = nakama:new(id, User:id(), Yuza:name()),
														    case NewNakama:save() of
															{ok, _} ->
															    ok;
															{error, [ErrorMessages]} ->
															    error_logger:info_msg(ErrorMessages)
														    end
													    end,
													    Yuzas)
										      end,
										      Dads)
								end,
								OtherNakamas)
					  end
				  end,
				  Json),
		    case lists:all(fun(Name) ->
					   OtherNakama = other_nakama:new(id, Id, Name),
					   case OtherNakama:save() of
					       {error, _} ->
						   false;
					       {ok, _}  ->
						   true
					   end
				   end,
				   Json) of
			false ->
			    {json, [{error, "Create friend error"}]};
			true ->
			    {json, [{error, Json}]}
		    end
	    end
    end;
    %% case Req:param("friends") of
    %%	undefined ->
    %%	    {json, [{error, "Need friends"}]};
    %%	Friends ->
    %%	    %Json = jsx:decode(Friends),
    %%	    {json, [{error, Friends}]}
    %% end;

update(_, [Id], _) ->
    {json, [{error, "Not supported"}]}.

nakama('GET', [Id], User) ->
    case User:nakamas() of
	[] ->
	    {json, [{error, "Empty"}]};
	Nakamas ->
	    case Id of
		"all" ->
		    {json, [{error, "Ok"},
			    {nakamas, Nakamas}
			   ]};
		_ ->
		    case User:first_nakama([{id, 'equals', Id}]) of
			undefined ->
			    {json, [{error, "Bad id"}]};
			Nakama ->
			    {json, [{error, "OK"},
				    {nakama, Nakama}
				   ]}
		    end
	    end
    end;

nakama(_,[Id], _) ->
    {json, [{error, "Not supported"}]}.
