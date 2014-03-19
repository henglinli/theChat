-module(yuza, [Id, 
	       Email::string(),
	       Name::string(), 
	       RegisterTime::datetime(),
	       Password::binary()]).
%
-compile(export_all).
%
-has({owned_accounts, many}).
%
-has({nakamas, many}).
