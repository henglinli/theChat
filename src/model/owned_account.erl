-module(owned_account, 
	[Id,
	 YuzaId,
	 Name,
	 Token]).

-compile(export_all).

-belongs_to(yuza).
