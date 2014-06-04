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
    case Req:post_param("password") of
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
			   utils:account_types()) of
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
							    {json, [{error, "OK"}
								    %{account, SavedAccount:attributes()}
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
	    {json, [{error, "OK"},
		    {owned_accounts, []}
		   ]};
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

hello('POST', [], User) ->
     case Req:param("nakamas") of
	 undefined ->
	     {json, [{error, "Need nakamas"}]};
	 Nakamas ->
	     Friends = erlang:list_to_binary(Nakamas),
	     {json, [{error, "OK"},
		     {hello, jsx:decode(Friends)}]}
     end;

hello(_, [], _) ->
     {json, [{error, "Not supported"}]}.

friend('GET', [Id], User) ->
    case Id of
	"all" ->
	    %% {json, [{error, "OK"}]};
	    case User:owned_accounts() of
		[] ->
		    {json, [{error, "Empty"}]};
		Accounts ->
		    Attributes = lists:map(fun(Account) ->
						   OtherNakamas = lists:map(fun(OtherNakama) ->
										    OtherNakama:attributes()
									    end,
									    Account:other_nakamas()),
						   lists:append([Account:attributes(), [{other_nakamas, OtherNakamas}]])
					   end,
					   Accounts),
		    {json, [{error, "Ok"},
			    {owned_accounts, Attributes}
			   ]};
		Other ->
		    {json, [{error, Other}]}
	    end;
	_ ->
	    case User:first_owned_account([{id, 'equals', Id}]) of
		undefined ->
		    {json, [{error, "Bad id"}]};
		OwnedAccount ->
		    {json, [{error, "OK"}]}
	    end
    end;

friend('POST', [Id], User) ->
    %% {json, [{error, Req:request_body()}]};
    case User:first_owned_account([{id, 'equals', Id}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	OwnedAccount ->
	    case Req:param("friends") of
		undefined ->
		    {json, [{error, "Need nakamas"}]};
		Nakamas ->
		    Friends = erlang:list_to_binary(Nakamas),
		    case jsx:decode(Friends) of
			{incomplete, _} ->
			    {json, [{error, "Bad Nakamas format"}]};
			[] ->
			    {json, [{error, "Empty Nakamas"}]};
			Json ->
			    lager:info("Json: ~p",[Json]),
			    lists:foreach(fun(Name) ->
						  case boss_db:find(owned_account, [{name, 'equals', erlang:binary_to_list(Name)}]) of
						      {error, Reason} ->
							  lager:error("boss_db find error: ~p", [Reason]),
							  false;
						      [] ->
							  continue;
						      OwnedAccounts ->
							  lists:foreach(fun(OwnedAccount) ->
										Dads = OwnedAccount:belongs_to(),
										lists:foreach(fun(Dad) ->
												      {yuza, Yuza} = Dad,
												      utils:make_nakama(User, Yuza)
											      end,
											      Dads)
									end,
									OwnedAccounts)
						  end
					  end,
					  Json),
			    %% find each name in other_nakama
			    case lists:all(fun(Name) ->
						  case boss_db:find(other_nakama, [{name, 'equals', Name},
										   {owned_account_id, 'equals', Id}]) of
						      {error, Reason} ->
							  lager:error("boss_db find error: ~p", [Reason]),
							  false;
						      [] ->
							  lager:info("not found: ~p", [Name]),
							  NewOtherNakama = other_nakama:new(id, Id, Name),
							  case NewOtherNakama:save() of
							      {error, ErrorMessages} ->
								  lager:error("boss_db save error: ~p", [ErrorMessages]),
								  false;
							      {ok, _}  ->
								  true
							  end;
						      OtherNakamas ->
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
	    end
    end;
    %% case Req:param("friends") of
    %%	undefined ->
    %%	    {json, [{error, "Need friends"}]};
    %%	Friends ->
    %%	    %Json = jsx:decode(Friends),
    %%	    {json, [{error, Friends}]}
    %% end;
friend('PUT', [Id], User) ->
    case User:first_owned_account([{id, 'equals', Id}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	OwnedAccountId ->
	    case jsx:decode(Req:request_body()) of
		{incomplete, _} ->
		    {json, [{error, "Bad request body"}]};
		[] ->
		    {json, [{error, "Bad request body"}]};
		Json ->
		    lager:info("Json: ~p",[Json]),
		    Json
	    end
    end;

friend('DELETE', [Id], User) ->
    case User:first_owned_account([{id, 'equals', Id}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	OwnedAccountId ->
	    case jsx:decode(Req:request_body()) of
		{incomplete, _} ->
		    {json, [{error, "Bad request body"}]};
		[] ->
		    {json, [{error, "Bad request body"}]};
		Json ->
		    Json
	    end
    end;

friend(_, [_], _) ->
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

nakama(_,[_], _) ->
    {json, [{error, "Not supported"}]}.
