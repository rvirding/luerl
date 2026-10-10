%% Copyright (C) 2025 Robert Virding, Dave Lucia
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(luerl_string_find_tests).

-include_lib("eunit/include/eunit.hrl").

%% A plain find of the empty string matches at the start position, as in
%% Lua 5.3, rather than crashing with badarg from binary:match/3.

find(Args) ->
    {ok,Ret,_} = luerl:call_function_dec([<<"string">>,<<"find">>], Args, luerl:init()),
    Ret.

plain_empty_test_() ->
    [?_assertEqual([1,0], find([<<"abc">>,<<>>,1,true])),
     ?_assertEqual([3,2], find([<<"abc">>,<<>>,3,true])),
     ?_assertEqual([4,3], find([<<"abc">>,<<>>,4,true])),
     ?_assertEqual([nil], find([<<"abc">>,<<>>,5,true])),
     ?_assertEqual([3,2], find([<<"abc">>,<<>>,-1,true])),
     ?_assertEqual([1,0], find([<<>>,<<>>,1,true])),
     %% The pattern search agrees.
     ?_assertEqual([3,2], find([<<"abc">>,<<>>,3])),
     %% And non-empty plain searches are unchanged.
     ?_assertEqual([2,3], find([<<"a.c.">>,<<".c">>,1,true]))].
