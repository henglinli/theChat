-module(owned_account, 
	[Id,
	 YuzaId,
	 Name::string(),
	 Token::string()]).

-compile(export_all).

-belongs_to(yuza).
