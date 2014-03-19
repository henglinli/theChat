-module(comment, [Id, 
		  User,
		  Replay, % replay who
		  At,
		  Body::string(),
		  Time::datetime()
			]).
-compile(export_all).
