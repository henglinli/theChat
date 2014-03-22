-module(other_nakama,
	[Id,
	 OwnedAccountId,
	 Name
	]).

-compile(export_all).

-belongs_to(owned_account).
