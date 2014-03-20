-module(yuza, [Id, 
	       Email::string(),
	       Name::string(), 
	       RegisterTime::datetime(),
	       Password::non_neg_integer()]).
%
-compile(export_all).
%
-has({owned_accounts, many}).
%
-has({nakamas, many}).
