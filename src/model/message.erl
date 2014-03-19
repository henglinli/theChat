-module(message,
	[Id,
	 From,
	 Message::string(),
	 Time::non_neg_integer()]).

-compile(export_all).
