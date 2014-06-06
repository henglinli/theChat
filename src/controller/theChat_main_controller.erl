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
deregister('DELETE', [Password], User) ->
    case utils:check_password(User:password(), Password) of
	false ->
	    {json, [{error, "Bad password"}]};
	true ->
	    case boss_db:delete(User:id()) of
		ok ->
		    {json, [{error, "OK"}]};
			{error, Reason} ->
		    {json, [{error, Reason}]}
	    end
    end;

deregister(_, [], _) ->
    {json, [{error, "Not supported"}]}.

profile('GET', [], User) ->
    UserAttributes = User:attributes(),
    UserInfo = lists:sublist(UserAttributes, 4),
    {json, [{error, "OK"},
	    {profile, UserInfo}
	    ]};

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

friend('GET', [OwnedAccountId], User) ->
    case OwnedAccountId of
	"all" ->
	    %% {json, [{error, "OK"}]};
	    case User:owned_accounts() of
		[] ->
		    {json, [{error, "Empty"}]};
		OwnedAccounts ->
		    Attributes = lists:map(fun(OwnedAccount) ->
						   OtherNakamasAttibutes = lists:map(fun(OtherNakama) ->
										    OtherNakama:attributes()
									    end,
									    OwnedAccount:other_nakamas()),
						   lists:append([OwnedAccount:attributes(), [{other_nakamas, OtherNakamasAttibutes}]])
					   end,
					   OwnedAccounts),
		    {json, [{error, "Ok"},
			    {owned_accounts, Attributes}
			   ]};
		Others ->
		    {json, [{error, Others}]}
	    end;
	_ ->
	    case User:first_owned_account([{id, 'equals', OwnedAccountId}]) of
		undefined ->
		    {json, [{error, "Bad id"}]};
		OwnedAccount ->
		    OtherNakamas = lists:map(fun(OtherNakama) ->
						     OtherNakama:attributes()
					     end,
					     OwnedAccount:other_nakamas()),
		    Attributes  = lists:append([OwnedAccount:attributes(), [{other_nakamas, OtherNakamas}]]),
		    {json, [{error, "OK"},
			    {owned_account, Attributes}
			   ]};
		Others ->
		    {json, [{error, Others}]}
	    end
    end;

%% friend('POST', [Id], User) ->
%%     %% {json, [{error, Req:request_body()}]};
%%     case User:first_owned_account([{id, 'equals', Id}]) of
%%	undefined ->
%%	    {json, [{error, "Bad id"}]};
%%	OwnedAccount ->
%%	    case Req:param("friends") of
%%		undefined ->
%%		    {json, [{error, "Need friends"}]};
%%		Nakamas ->
%%		    Friends = erlang:list_to_binary(Nakamas),
%%		    case jsx:decode(Friends) of
%%			{incomplete, _} ->
%%			    {json, [{error, "Bad friends format"}]};
%%			[] ->
%%			    {json, [{error, "Empty friends"}]};
%%			Json ->
%%			    lager:info("Json: ~p",[Json]),
%%			    lists:foreach(fun(Name) ->
%%						  case boss_db:find(owned_account, [{name, 'equals', erlang:binary_to_list(Name)}]) of
%%						      {error, Reason} ->
%%							  lager:error("boss_db find error: ~p", [Reason]),
%%							  false;
%%						      [] ->
%%							  continue;
%%						      OwnedAccounts ->
%%							  lists:foreach(fun(OwnedAccount) ->
%%										Dads = OwnedAccount:belongs_to(),
%%										lists:foreach(fun(Dad) ->
%%												      {yuza, Yuza} = Dad,
%%												      utils:make_nakama(User, Yuza)
%%											      end,
%%											      Dads)
%%									end,
%%									OwnedAccounts)
%%						  end
%%					  end,
%%					  Json),
%%			    %% find each name in other_nakama
%%			    case lists:all(fun(Name) ->
%%						  case boss_db:find(other_nakama, [{name, 'equals', erlang:binary_to_list(Name)},
%%										   {owned_account_id, 'equals', Id}]) of
%%						      {error, Reason} ->
%%							  lager:error("boss_db find error: ~p", [Reason]),
%%							  false;
%%						      [] ->
%%							  lager:info("not found: ~p", [Name]),
%%							  NewOtherNakama = other_nakama:new(id, Id, Name),
%%							  case NewOtherNakama:save() of
%%							      {error, ErrorMessages} ->
%%								  lager:error("boss_db save error: ~p", [ErrorMessages]),
%%								  false;
%%							      {ok, _}  ->
%%								  true
%%							  end;
%%						      OtherNakamas ->
%%							  true
%%						  end
%%					   end,
%%					   Json) of
%%				false ->
%%				    {json, [{error, "Create friend error"}]};
%%				true ->
%%				    {json, [{error, Json}]}
%%			    end
%%		    end
%%	    end
%%     end;

%% friend('PUT', [Id], User) ->
%%     case User:first_owned_account([{id, 'equals', Id}]) of
%%	undefined ->
%%	    {json, [{error, "Bad id"}]};
%%	OwnedAccount ->
%%	     case Req:param("friends") of
%%		undefined ->
%%		    {json, [{error, "Need friends"}]};
%%		Nakamas ->
%%		    Friends = erlang:list_to_binary(Nakamas),
%%		    case jsx:decode(Friends) of
%%			{incomplete, _} ->
%%			    {json, [{error, "Bad friends format"}]};
%%			[] ->
%%			    {json, [{error, "Empty friends"}]};
%%			Json ->
%%			    lager:info("Json: ~p",[Json]),
%%			    NewFriends = lists:filter(fun(Name) ->
%%							      case boss_db:find(owned_account, [{name, 'equals', erlang:binary_to_list(Name)}]) of
%%								  {error, Reason} ->
%%								      lager:error("boss_db find error: ~p", [Reason]),
%%								      false;
%%								  [] ->
%%								      %% no firends
%%								      true;
%%								  OwnedAccounts ->
%%								      lists:foreach(fun(OwnedAccount) ->
%%											    case OwnedAccount:id() of
%%												Id ->
%%												    %% is me
%%												    false;
%%												_ ->
%%												    %% make friend
%%												    Dads = OwnedAccount:belongs_to(),
%%												    lists:all(fun(Dad) ->
%%														      {yuza, Yuza} = Dad,
%%														      utils:make_nakama(User, Yuza)
%%													      end,
%%													      Dads)
%%											    end
%%										    end,
%%										    OwnedAccounts)
%%							      end
%%						      end,
%%						      Json),
%%			    case lists:all(fun(Friend) ->
%%						   NewOtherNakama = other_nakama:new(id, Id, Friend),
%%						   case NewOtherNakama:save() of
%%						       {error, ErrorMessages} ->
%%							   lager:error("boss_db save error: ~p", [ErrorMessages]),
%%							   false;
%%						       {ok, _}  ->
%%							   true
%%						   end
%%					   end,
%%					   NewFriends) of
%%				false ->
%%				    {json, [{error, "add friend error"}]};
%%				true ->
%%				    {json, [{error, NewFriends}]}
%%			    end
%%		    end
%%	     end
%%     end;

friend('PUT', [OwnedAccountId, OtherNakamaName], User) ->
    case User:first_owned_account([{id, 'equals', OwnedAccountId}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	OwnedAccount ->
	    case OwnedAccount:first_other_nakama([{name, 'equals', OtherNakamaName}]) of
		undefined -> %% not found and can add
		    NewOtherNakama = other_nakama:new(id, OwnedAccountId, OtherNakamaName),
		    case NewOtherNakama:save() of
			{error, ErrorMessages} ->
			    lager:error("boss_db save error: ~p", [ErrorMessages]),
			    {json, [{error, ErrorMessages}]};
			{ok, SavedOtherNakama}  ->
			    case boss_db:find(owned_account, [{name, 'equals', OtherNakamaName},
							      {yuza_id, 'not_equals', User:id()}]) of
				{error, Reason} ->
				    lager:error("boss_db find error: ~p", [Reason]),
				    {json, [{error, Reason}]};
				[] ->
				    {json, [{error, "Done"},
					    {result, SavedOtherNakama:attributes()}]};
				OwnedAccounts -> %% found friend
				   case lists:all(fun(TheOwnedAccount) ->
							     %% make friend
							     Dads = TheOwnedAccount:belongs_to(),
							     lists:all(fun(Dad) ->
									       {yuza, Yuza} = Dad,
									       utils:make_nakama(User, Yuza)
								       end,
								       Dads)
						     end,
						     OwnedAccounts) of
				       false ->
					   {json, [{error, "Create friend error"}]};
				       true ->
					   {json, [{error, "OK"},
						   {result, SavedOtherNakama:attributes()}]}
				   end
			    end
		    end;
		OtherNakama ->
		    {json, [{error, "Already added"}]}
	    end
    end;

friend('DELETE', [OwnedAccountId, OtherNakamaName], User) ->
    case User:first_owned_account([{id, 'equals', OwnedAccountId}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	OwnedAccount ->
	    case OwnedAccount:first_other_nakama([{name, 'equals', OtherNakamaName}]) of
		 undefined -> %% not found, bad name
		     OtherNakamas = OwnedAccount:other_nakamas(),
		     {json, [{error, "Bad name"}
			     ,{owned_account, OwnedAccount:attributes()}
			     ,{other_nakamas, lists:map(fun(OtherNakama) ->
								OtherNakama:attributes()
							end,
							OtherNakamas)}
			    ]};
		 OtherNakama ->
		     case boss_db:find(owned_account, [{name, 'equals', OtherNakamaName},
						       {yuza_id, 'not_equals', User:id()}]) of
			 {error, Reason} ->
			     lager:error("boss_db find error: ~p", [Reason]),
			     {json, [{error, Reason}]};
			 [] -> %% not found firend, act delete
			     case boss_db:delete(OtherNakama:id()) of
				 {error, Reason} ->
				     {json, [{error, Reason}]};
				 ok ->
				     {json, [{error, "OK"}]}
			     end;
			OwnedAccounts -> %% found friend
			    case lists:all(fun(TheOwnedAccount) ->
						   %% make friend
						   Dads = TheOwnedAccount:belongs_to(),
						   lists:all(fun(Dad) ->
								     {yuza, Yuza} = Dad,
								     utils:delete_nakama(User, Yuza)
							     end,
							     Dads)
					   end,
					   OwnedAccounts) of
				false ->
				    {json, [{error, "delete friend error"}]};
				true ->
				    case boss_db:delete(OtherNakama:id()) of
					{error, Reason} ->
					    {json, [{error, Reason}]};
					ok ->
					    {json, [{error, "OK"}]}
				    end
			    end
		    end
	    end
    end;

friend(_, [_], _) ->
    {json, [{error, "Not supported"}]}.

delete_friend('POST', [Id], User) ->
    case User:first_owned_account([{id, 'equals', Id}]) of
	undefined ->
	    {json, [{error, "Bad id"}]};
	OwnedAccount ->
	     case Req:param("friends") of
		undefined ->
		    {json, [{error, "Need friends"}]};
		Nakamas ->
		    Friends = erlang:list_to_binary(Nakamas),
		    case jsx:decode(Friends) of
			{incomplete, _} ->
			    {json, [{error, "Bad friends format"}]};
			[] ->
			    {json, [{error, "Empty friends"}]};
			Json ->
			    lager:info("Json: ~p",[Json]),
			    %% find can delete
			    OldFriends = lists:filter(fun(Name) ->
							      case boss_db:find(other_nakama, [{name, 'equals', erlang:binary_to_list(Name)},
											       {owned_account_id, 'equals', Id}]) of
								  {error, Reason} ->
								      lager:error("boss_db find error: ~p", [Reason]),
								      false;
								  [] ->
								      lager:info("not found: ~p id: ~p", [Name, Id]),
								      false;
								  OtherNakamas ->
								      lager:info("found: ~p", [Name]),
								      lists:all(fun(OtherNakama) ->
											case boss_db:delete(OtherNakama:id()) of
											    {error, Reason} ->
												lager:error("boss_db delete error: ~p", [Reason]),
												false;
											    ok ->
												true;
											    _ ->
												false
											end
										end,
										OtherNakamas)
							      end
						      end,
						      Json),
			    case lists:all(fun(Name) ->
						   case boss_db:find(owned_account, [{name, 'equals', Name}]) of
						       {error, Reason} ->
							   lager:error("boss_db find error: ~p", [Reason]),
							   false;
						       [] ->
							   %% no firends
							   true;
						       OwnedAccounts ->
							   lists:all(fun(OwnedAccount) ->
									     case OwnedAccount:id() of
										 Id ->
										     %% is me
										     false;
										 _ ->
										     %% delete friend
										     Dads = OwnedAccount:belongs_to(),
										     lists:all(fun(Dad) ->
												       {yuza, Yuza} = Dad,
												       utils:delete_nakama(User, Yuza)
											       end,
											       Dads)
									     end
								     end,
								     OwnedAccounts)
						   end
					   end,
					   OldFriends) of
				false ->
				    {json, [{error, "Create friend error"}]};
				true ->
				    {json, [{error, Json}]}
			    end
		    end
	     end
    end;

delete_friend(_, [_], _) ->
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
