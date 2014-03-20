-module(yuza, [Id, 
	       Email,
	       Name,
	       RegisterTime,
	       Password]).
%
-compile(export_all).
%
-has({owned_accounts, many}).
%
-has({nakamas, many}).
