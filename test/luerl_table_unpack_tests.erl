%% Regression tests for the default end index of table.unpack.
-module(luerl_table_unpack_tests).
-include_lib("eunit/include/eunit.hrl").

plain_table_control_test() ->
    run(<<"local t = {10, 20, 30}\n"
          "local p = table.pack(table.unpack(t))\n"
          "assert(p.n == 3 and p[1] == 10 and p[3] == 30)\n"
          "local q = table.pack(table.unpack(t, 2, 4))\n"
          "assert(q.n == 3 and q[1] == 20 and q[2] == 30 and q[3] == nil)\n"
          "assert(select(\"#\", table.unpack({})) == 0)">>).

direct_length_control_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10, 20, 30}, {__len = function(self)\n"
          "  calls = calls + 1\n"
          "  assert(rawget(self, 1) == 10)\n"
          "  return 1\n"
          "end})\n"
          "assert(#t == 1 and calls == 1)">>).

default_end_uses_length_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10, 20, 30}, {__len = function(self)\n"
          "  calls = calls + 1\n"
          "  assert(rawget(self, 1) == 10)\n"
          "  return 1\n"
          "end})\n"
          "local p = table.pack(table.unpack(t))\n"
          "assert(calls == 1 and p.n == 1 and p[1] == 10)">>).

nil_end_uses_length_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10, 20, 30}, {__len = function()\n"
          "  calls = calls + 1\n"
          "  return 2\n"
          "end})\n"
          "local p = table.pack(table.unpack(t, 2, nil))\n"
          "assert(calls == 1 and p.n == 1 and p[1] == 20)">>).

explicit_end_skips_length_test() ->
    run(<<"local t = setmetatable({10, 20, 30}, {__len = function()\n"
          "  error(\"length must not run\")\n"
          "end})\n"
          "local p = table.pack(table.unpack(t, 1, 2))\n"
          "assert(p.n == 2 and p[1] == 10 and p[2] == 20)\n"
          "local q = table.pack(table.unpack(t, nil, 1))\n"
          "assert(q.n == 1 and q[1] == 10)\n"
          "assert(select(\"#\", table.unpack(t, 2, 1)) == 0)">>).

length_mutation_is_visible_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10, 20, 30}, {__len = function(self)\n"
          "  calls = calls + 1\n"
          "  self[-1] = -10\n"
          "  self[0] = 0\n"
          "  self[1] = 99\n"
          "  self[2] = nil\n"
          "  return 2\n"
          "end})\n"
          "local p = table.pack(table.unpack(t, -1))\n"
          "assert(calls == 1 and p.n == 4)\n"
          "assert(p[1] == -10 and p[2] == 0 and p[3] == 99 and p[4] == nil)\n"
          "assert(rawget(t, 1) == 99 and rawget(t, 2) == nil)">>).

zero_length_returns_no_values_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10}, {__len = function()\n"
          "  calls = calls + 1\n"
          "  return 0\n"
          "end})\n"
          "assert(select(\"#\", table.unpack(t)) == 0 and calls == 1)">>).

negative_length_returns_no_values_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10}, {__len = function()\n"
          "  calls = calls + 1\n"
          "  return -1\n"
          "end})\n"
          "assert(select(\"#\", table.unpack(t)) == 0 and calls == 1)">>).

length_error_propagates_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10}, {__len = function()\n"
          "  calls = calls + 1\n"
          "  error(\"length sentinel\", 0)\n"
          "end})\n"
          "local ok, message = pcall(table.unpack, t)\n"
          "assert(ok == false and message == \"length sentinel\" and calls == 1)">>).

integral_float_length_test() ->
    run(<<"local t = setmetatable({10, 20, 30}, {__len = function()\n"
          "  return 2.0\n"
          "end})\n"
          "local p = table.pack(table.unpack(t))\n"
          "assert(p.n == 2 and p[1] == 10 and p[2] == 20)">>).

numeric_string_length_test() ->
    run(<<"local t = setmetatable({10, 20, 30}, {__len = function()\n"
          "  return \"2\"\n"
          "end})\n"
          "local p = table.pack(table.unpack(t))\n"
          "assert(p.n == 2 and p[1] == 10 and p[2] == 20)">>).

invalid_length_is_lua_error_test() ->
    run(<<"local function check(value)\n"
          "  local calls = 0\n"
          "  local t = setmetatable({10}, {__len = function()\n"
          "    calls = calls + 1\n"
          "    return value\n"
          "  end})\n"
          "  local ok = pcall(table.unpack, t)\n"
          "  assert(ok == false and calls == 1)\n"
          "end\n"
          "check(nil)\n"
          "check(false)\n"
          "check(\"not a length\")\n"
          "check({})">>).

fractional_length_is_lua_error_test() ->
    run(<<"local calls = 0\n"
          "local t = setmetatable({10, 20, 30}, {__len = function()\n"
          "  calls = calls + 1\n"
          "  return 1.5\n"
          "end})\n"
          "local ok = pcall(table.unpack, t)\n"
          "assert(ok == false and calls == 1)">>).

run(Chunk) ->
    {Status,Result,_State} = luerl:do(Chunk, luerl:init()),
    ?assertEqual({ok,[]}, {Status,Result}).
