%% We include the whole environment in one structure even if fields
%% come from logically different parts. This make it easier to pass
%% around but does mean that there will be more explicit fiddleling to
%% get it right. See block/2 and functioncall/4 for examples of this.

-record(luerl, {tabs,free,next,			%Table structure
		env,				%Environment
		locf=false,			%Started local function
		tag				%Unique tag
	       }).

%% -record(etab, {tabs=[],free=[],next=0}).	%Tables structure
%% -record(eenv, {env=[]}).			%Environment
%% -record(luerl, {tabs,env}).			%Full state

-define(IS_INTEGER(N), (float(round(N)) =:= N)).
-define(IS_TRUE(X), (((X) =/= nil) and ((X) =/= false))).

-define(USE_ORDDICT, true).

-ifdef(USE_ORDDICT).
%% Using orddict to handle tables.
-define(MAKE_TABLE(), orddict:new()).
-define(GET_TABLE(N, Ts), orddict:fetch(N, Ts)).
-define(SET_TABLE(N, T, Ts), orddict:store(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), orddict:update(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), orddict:erase(N, Ts)).
-define(FILTER_TABLE(Pred, Ts), orddict:filter(Pred, Ts)).
-define(FOLD_TABLE(Fun, Acc, Ts), orddict:fold(Fun, Acc, Ts)).
-endif.

-ifdef(USE_ARRAY).
%% Use arrays to handle tables.
-define(MAKE_TABLE(), array:new()).
-define(GET_TABLE(N, Ar), array:get(N, Ar)).
-define(SET_TABLE(N, T, Ar), array:set(N, T, Ar)).
-define(UPD_TABLE(N, Upd, Ar),
	array:set(N, (Upd)(array:get(N, Ar)), Ar)).
-define(DEL_TABLE(N, Ar), array:reset(N, Ar)).
-define(FILTER_TABLE(Pred, Ar),
	((fun (___Def) ->
		  Fil = fun (___K, ___V) ->
				case Pred(___K, ___V) of
				    true -> ___V;
				    false -> ___Def
				end
			end,
		  array:sparse_map(Fil, Ar)
	  end)(array:default(Ar)))).
-define(FOLD_TABLE(Fun, Acc, Ar), array:sparse_foldl(Fun, Acc, Ar)).

-endif.
