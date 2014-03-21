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
