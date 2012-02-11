%% We include the whole environment in one structure even if fields
%% come from logically different parts. This make it easier to pass
%% around but does mean that there will be more explicit fiddleling to
%% get it right. See block/2 and functioncall/4 for examples of this.

-record(luerl, {tabs,free,next,			%Table structure
		env,				%Environment
		sub=false			%Started local function
	       }).

%% -record(etab, {tabs=[],free=[],next=0}).	%Tables structure
%% -record(eenv, {env=[]}).			%Environment
%% -record(luerl, {tabs,env}).			%Full state

-define(IS_INTEGER(N), (float(round(N)) =:= N)).
-define(IS_TRUE(X), (((X) =/= nil) and ((X) =/= false))).

%% Using orddict to handle tables.
-define(GET_TABLE(N, Ts), orddict:fetch(N, Ts)).
-define(SET_TABLE(N, T, Ts), orddict:store(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), orddict:update(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), orddict:erase(N, Ts)).
