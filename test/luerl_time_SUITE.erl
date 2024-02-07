-module(luerl_time_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([os_date/1]).

-if(?OTP_RELEASE >= 25).
-define(START_NODE(Name, Env), (fun(Name, Env) -> 
    case ?CT_PEER(#{name => Name, env => Env}) of
        {ok, _PeerPid, Node} ->
            {ok, Node};
        Err = {error, _Reason, _NodeName} ->
            Err
    end
end)(Name, Env)).
-else.
-define(START_NODE(Name, Env), (fun(Name, Env) -> 
    ct_slave:start(Name, [{env, Env}, {monitor_master, true}])
end)(Name, Env)).
-endif.

all() ->
  lists:flatten([windows_tests(), linux_tests()]).

os_date(_Config) ->
    {ok, LusakaNode} = ?START_NODE(africa_lusaka, [{"TZ", "Africa/Lusaka"}]),
    ok = set_path(LusakaNode),
    {ok, LondonNode} = ?START_NODE(europe_london, [{"TZ", "Europe/London"}]),
    ok = set_path(LondonNode),
    LusakaLocalTime = rpc:call(LusakaNode, calendar, local_time, []),
    LondonLocalTime = rpc:call(LondonNode, calendar, local_time, []),
    ?assertNotMatch({badrpc, _}, LusakaLocalTime),
    ?assertNotMatch({badrpc, _}, LondonLocalTime),
    ?assert(LusakaLocalTime =/= LondonLocalTime),
    ?assertEqual({ok,[<<"Sat May  6 13:16:07 2023">>]}, rpc:call(LusakaNode, luerl, eval, ["return os.date('%c', 1683371767)", luerl:init()])),
    ?assertEqual({ok,[<<"Sat May  6 12:16:07 2023">>]}, rpc:call(LondonNode, luerl, eval, ["return os.date('%c', 1683371767)", luerl:init()])),
    ok.

windows_tests() ->
    [].

linux_tests() ->
    [os_date].

set_path(Node) ->
    [case rpc:call(Node, code, add_path, [Path]) of
        true ->
            ok;
        Err = {error, _} ->
            throw({badpath, Path, Err})
    end || Path <- code:get_path(), filelib:is_dir(Path)],
    ok.