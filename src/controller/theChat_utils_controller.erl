-module(theChat_utils_controller, [Req]).
-compile(export_all).

version(_, []) ->
    {json, [{version, "2014-03-26"},
	    {copyright, "nil"},
	    {author, "henglinli@gmail.com"}]}.

nope('GET', []) ->
    {json, [{error, "Not found"}]};

nope(_, []) ->
    {json, [{error, "Not support"}]}.
    
oops('GET', []) ->
    {json, [{error, "Internal error"}]};

oops(_, []) ->
    {json, [{error, "Not support"}]}.
