-module(twitter, [Id, 
		  User, % who twitt it
		  At,
		  Liked,
		  Body::string(),
		  Time::datetime(),
		  From::string() % From what device
			]).
-compile(export_all).
