-module(comment, [Id,
		  YuzaId,
		  From,
		  Comment,
		  FromWho,
		  FromWhich,
		  ToWho
		 ]).
%
-compile(export_all).
%
-belongs_to(yuza).
