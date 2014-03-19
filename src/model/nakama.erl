-module(nakama, 
	[Id,
	 YuzaId,
	 Name::string()]).

-compile(export_all).

-belongs_to(yuza).
