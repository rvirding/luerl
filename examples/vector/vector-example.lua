-- File    : vector-example.lua
-- Purpose : Brief demonstration of a 2D vector library on Luerl.
-- See     : ./vector.erl


local vector = require("vector")

local v = vector(100, 100) -- you can also use vector.new()
print(v) -- will print "(100, 100)"

print(vector.random())