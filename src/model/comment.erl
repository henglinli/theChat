-module(comment, [Id,
		  YuzaId,
		  From,
		  Comment,
		  FromWho,
		  FromWhich,
		  ToWho,
		  Touched
		 ]).
%
-compile(export_all).
%
-belongs_to(yuza).
