-- File    : json-example.lua
-- Purpose : Brief demonstration of JSON.lua on Luerl.
-- See     : ./json.erl


local json = require("JSON")

local json_string = '{"city":"Kyoto","climate":{"avg_temp":16,"humidity":"high"},"country":"Japan"}'

local foo = json:decode(json_string)

foo['city'] = "Alajuela"
foo['country'] = "Costa Rica"
foo['climate']['avg_temp'] = 27

local new_string = json:encode(foo)

print(json:encode_pretty(foo))