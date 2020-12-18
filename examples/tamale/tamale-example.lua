-- File    : tamale-example.lua
-- Purpose : Brief demonstration of tamale on Luerl.
-- See     : ./json.erl


local tamale = require('tamale')
-- Logical variable.
local V = tamale.var
-- Creating the matching function.
local M = tamale(
  {{{ 'foo', 1, {} }, 'one' },
   { 10, function() return 'two' end },
   {{ 'bar', 10, 100 }, 'three' },
   {{ 'baz', V'X' }, V'X' }, -- V'X' is a variable
   {{ 'add', V'X', V'Y' },  function(cs) return cs.X + cs.Y end }})
-- Trying tamale
print(M( {'foo', 1, {} }))   --> 'one'
print(M(10))                 --> 'two'
print(M({ 'bar', 10, 100 })) --> 'three'
print(M({ 'baz', 'four' }))  --> 'four'
print(M({ 'add', 2, 3 }))     --> 5
print(M({ 'sub', 2, 3 }))     --> nil, 'Match failed'