-module(comment, [Id,
		  YuzaId,
		  YuzaName,
		  Comment,
		  FromId,
		  FromWhich,		  
		  FromWho
		 ]).
%
-compile(export_all).
%
-belongs_to(yuza).
