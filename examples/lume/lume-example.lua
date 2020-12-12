-- File    : lume-example.lua
-- Purpose : Brief demonstration of lume on Luerl.
-- See     : ./lume.erl

local lume = require("lume")

print( lume.clamp(8, 5, 10),       8     )
print( lume.clamp(12, 5, 10),      10    )
print( lume.clamp(-1, 5, 10),      5     )
print( lume.clamp(-1, -10, 10),    -1    )
print( lume.clamp(-100, -10, 10),  -10   )
print( lume.clamp(13, 8, 8),       8     )
print( lume.clamp(3, 8, 8),        8     )

print( lume.round(.5),           1       )
print( lume.round(-.5),          -1      )
print( lume.round(2.4),          2       )
print( lume.round(123, 10),      120     )
print( lume.round(129, 64),      128     )
print( lume.round(-123.45, .1),  -123.5  )
print( lume.round(0),            0       )

print( lume.sign(-10),  -1 )
print( lume.sign(10),   1  )
print( lume.sign(0),    1  )

print( lume.lerp(100, 200, .5),    150  )
print( lume.lerp(100, 200, .25),   125  )
print( lume.lerp(100, 200, 2),     200  )
print( lume.lerp(100, 200, -2),    100  )

print( lume.smooth(100, 200, .5),  150  )
print( lume.smooth(100, 200, 0),   100  )
print( lume.smooth(100, 200, 1),   200  )
print( lume.smooth(100, 200, 2),   200  )
print( lume.smooth(100, 200, -2),  100  )

print( lume.pingpong(0),     0   )
print( lume.pingpong(1.5),   .5  )
print( lume.pingpong(-.2),   .2  )
print( lume.pingpong(-1.6),  .4  )
print( lume.pingpong(1.8),   .2  )

print( lume.distance(15, 20, 15, 20),        0             )
print( lume.distance(13, 44, 156, 232),      236.205419074 )
print( lume.distance(-23, 66, -232, 123),    216.633330769 )
local x = lume.distance(13, 15, -2, 81)
print( lume.distance(13, 15, -2, 81, true),  x * x         )

print( lume.angle(10, 10, 10, 10), math.rad(0)   )
print( lume.angle(10, 10, 20, 10), math.rad(0)   )
print( lume.angle(10, 10, 5,  10), math.rad(180) )
print( lume.angle(10, 10, 20, 20), math.rad(45)  )
print( lume.angle(10, 10, 10, 30), math.rad(90)  )

local function cmp(a, b) return math.abs(a - b) < 10e-6 end
local x, y
x, y = lume.vector(0, 10)
print( cmp(x, 10) and cmp(y, 0), true )
x, y = lume.vector(math.pi, 100)
print( cmp(x, -100) and cmp(y, 0), true )
x, y = lume.vector(math.pi * 0.25, 100)
print( cmp(x, 70.71067811865476) and cmp(y, 70.71067811865476), true )

print( type(lume.random()),      "number" )
print( type(lume.random(1)),     "number" )
print( type(lume.random(1, 2)),  "number" )

local t = {}
for i = 0, 1000 do
t[lume.randomchoice({"a", "b", "c", "d"})] = true
end
print( t.a and t.b and t.c and t.d,  true )
print( lume.randomchoice({true}),    true )

print( lume.weightedchoice( {a = 1} ),         "a" )
print( lume.weightedchoice( {a = 0, b = 1} ),  "b" )

local t = { 1, 2 }
lume.push(t, 3, 4)
print(t, { 1, 2, 3, 4 })
lume.push(t, 5, nil, 6, nil, 7)
print(t, { 1, 2, 3, 4, 5, 6, 7 })
lume.push(t)
print(t, { 1, 2, 3, 4, 5, 6, 7 })
local x, y = lume.push(t, 123, 456)
print(x, 123)
print(y, 456)

local t = { 1, 2, 3, 4, 5 }
lume.remove(t, 3)
print(t, { 1, 2, 4, 5  })
lume.remove(t, 1)
print(t, { 2, 4, 5 })
lume.remove(t, 5)
print(t, { 2, 4 })
local x = lume.remove(t, 123)
print(x, 123)

local t = { 1, 2, 3 }
lume.clear(t)
print(t, {})
local m = { a = 1, b = 2, c = 3 }
lume.clear(m)
print(m, {})
print( lume.clear(t) == t, true )

local t = { a = 10, b = 20, c = 30 }
print( lume.extend(t) == t, true )
lume.extend(t, { d = 40 }, { e = 50 })
print( t, { a = 10, b = 20, c = 30, d = 40, e = 50 } )
lume.extend(t, { a = "cat", b = "dog" }, { b = "owl", c = "fox" })
print( t, { a = "cat", b = "owl", c = "fox", d = 40, e = 50 } )

local t = {1, 2, 3, 4, 5}
t = lume.shuffle(t)
table.sort(t)
print( t,                {1, 2, 3, 4, 5} )
print( lume.shuffle({}), {}              )

local t = { 1, 5, 2, 4, 3 }
local fn = function(a, b) return a > b end
print( t == lume.sort(t), false             )
print( lume.sort(t),      { 1, 2, 3, 4, 5 } )
print( lume.sort(t, fn),  { 5, 4, 3, 2, 1 } )
print( t,                 { 1, 5, 2, 4, 3 } )
local t = { { id = 2 }, { id = 3 }, { id = 1 } }
print( lume.sort(t, "id"), { { id = 1 }, { id = 2 }, { id = 3 } })

local t = lume.array(pairs({a=0, b=0, c=0}))
table.sort(t)
print( t,                              {"a", "b", "c"} )
print( lume.array(ipairs({0, 0, 0})),  {1, 2, 3}       )

local acc = 1
lume.each({1, 2, 3}, function(x) acc = acc + x end)
print( acc, 7  )

local acc = 1
local f = function(o, x) acc = acc + x end
local f2 = function() end
local t = {a = {f = f}, b = {f = f}, c = {f = f2}}
lume.each(t, "f", 10)
print( acc, 21 )

print( lume.map({1, 2, 3}, function(x) return x * 2 end), {2, 4, 6} )
print( lume.map({a=2,b=3}, function(x) return x * 2 end), {a=4,b=6} )
local t = {{ id = 10 }, { id = 20 }, { id = 30 }}
print( lume.map(t, "id"), { 10, 20, 30 })

print( lume.all({true, true, false, true}),                        false )
print( lume.all({true, true, true, true}),                         true  )
print( lume.all({2, 3, 4, 5}, function(x) return x % 2 == 0 end),  false )
print( lume.all({2, 4, 6, 8}, function(x) return x % 2 == 0 end),  true  )
print( lume.all({{ x = 1 }, {}, { x = 3 }}, "x"),                  false )
print( lume.all({{ x = 1 }, { x = 2 }, { x = 3 }}, "x"),           true  )
print( lume.all({{ x = 1 }, { x = 2 }, { x = 3 }}, { x = 2 }),     false )
print( lume.all({{ x = 2 }, { x = 2 }, { x = 2 }}, { x = 2 }),     true  )

print( lume.any({true, true, false, true}),                        true  )
print( lume.any({false, false, false}),                            false )
print( lume.any({2, 3, 4, 5}, function(x) return x % 2 == 0 end),  true  )
print( lume.any({1, 3, 5, 7}, function(x) return x % 2 == 0 end),  false )
local t = {{ id = 10 }, { id = 20 }, { id = 30 }}
print( lume.any(t, { id = 10 }), true  )
print( lume.any(t, { id = 40 }), false )

local concat = function(a, b) return a .. b end
local add = function(a, b) return a + b end
local any = function(a, b) return a or b end
print( lume.reduce({"cat", "dog"}, concat, ""),    "catdog"    )
print( lume.reduce({"cat", "dog"}, concat, "pig"), "pigcatdog" )
print( lume.reduce({"me", "ow"}, concat),          "meow"      )
print( lume.reduce({1, 2, 3, 4}, add),             10          )
print( lume.reduce({1, 2, 3, 4}, add, 5),          15          )
print( lume.reduce({1}, add),                      1           )
print( lume.reduce({}, concat, "potato"),          "potato"    )
print( lume.reduce({a=1, b=2}, add, 5),            8           )
print( lume.reduce({a=1, b=2}, add),               3           )
print( lume.reduce({false, false, false}, any),    false       )
print( lume.reduce({false, true, false}, any),     true        )

print( lume.unique({}), {} )
local t = lume.unique({1, 2, 3, 2, 5, 6, 6})
table.sort(t)
print( t, {1, 2, 3, 5, 6} )
local t = lume.unique({"a", "b", "c", "b", "d"})
table.sort(t)
print( t, {"a", "b", "c", "d"} )

local t = lume.filter({1, 2, 3, 4, 5}, function(x) return x % 2 == 0 end  )
print( t, {2, 4} )
local t = lume.filter({a=1, b=2, c=3}, function(x) return x == 2 end, true)
print( t, {b=2} )
local t = lume.filter({{ x=1, y=1 }, { x=2, y=2 }, { x=1, y=3 }}, { x = 1 })
print( t, {{ x=1, y=1 }, {x=1, y=3}} )

local t = lume.reject({1, 2, 3, 4, 5}, function(x) return x % 2 == 0 end  )
print( t, {1, 3, 5} )
local t = lume.reject({a=1, b=2, c=3}, function(x) return x == 2 end, true)
print( t, {a=1, c=3} )
local t = lume.reject({{ x=1, y=1 }, { x=2, y=2 }, { x=1, y=3 }}, { x = 1 })
print( t, {{ x=2, y=2 }} )

print( lume.merge(),                       {}              )
print( lume.merge({x=1, y=2}),             {x=1, y=2}      )
print( lume.merge({a=1, b=2}, {b=3, c=4}), {a=1, b=3, c=4} )

print( lume.concat(nil),                            {}                    )
print( lume.concat({1, 2, 3}),                      {1, 2, 3}             )
print( lume.concat({1, 2, 3}, {4, 5, 6}),           {1, 2, 3, 4, 5, 6}    )
print( lume.concat({1, 2, 3}, {4, 5, 6}, nil, {7}), {1, 2, 3, 4, 5, 6, 7} )

print( lume.find({"a", "b", "c"}, "b"),  2   )
print( lume.find({"a", "b", "c"}, "c"),  3   )
print( lume.find({a=1, b=5, c=7}, 5),    "b" )

local t = { "a", "b", "c", "d" }
local t2 = { a = 1, b = 2, c = 3, d = 4 }
local t3 = { {x=1, y=2}, {x=3, y=4}, {x=5, y=6} }
local v, k = lume.match(t, function(x) return x > "c" end)
print( v, "d"  )
print( k, 4    )
local v, k = lume.match(t, function(x) return x < "b" end)
print( v, "a"  )
print( k, 1    )
local v, k = lume.match(t2, function(x) return x < 2 end)
print( v, 1    )
print( k, "a"  )
local v, k = lume.match(t2, function(x) return x > 5 end)
print( v, nil  )
print( k, nil  )
local v, k = lume.match(t3, { x = 3, y = 4 })
print( k, 2    )

local t = { a = 1, b = 2, c = 5, [13] = 22, z = 8 }
print( lume.count(t), 5 )
print( lume.count(t, function(x) return x % 2 == 0 end ), 3 )
local a = { 5, 6, 7, 8, 9 }
print( lume.count(a), #a )
local t = { { n = 20 }, { n = 30 }, { n = 40 }, { n = 20 } }
print( lume.count(t, { n = 20 }),  2 )
print( lume.count(t, { n = 30 }),  1 )
print( lume.count(t, { n = 50 }),  0 )

print( lume.slice({"a", "b", "c", "d", "e"}, 2, 4),    {"b", "c", "d"} )
print( lume.slice({"a", "b", "c", "d", "e"}, 2, -2),   {"b", "c", "d"} )
print( lume.slice({"a", "b", "c", "d", "e"}, 3, -1),   {"c", "d", "e"} )
print( lume.slice({"a", "b", "c", "d", "e"}, 3),       {"c", "d", "e"} )
print( lume.slice({"a", "b", "c", "d", "e"}, 4),       {"d", "e"}      )
print( lume.slice({"a", "b", "c", "d", "e"}, 1, 1),    {"a"}           )
print( lume.slice({"a", "b", "c", "d", "e"}, 2, 1),    {}              )
print( lume.slice({"a", "b", "c", "d", "e"}, -3, -2),  {"c", "d"}      )
print( lume.slice({"a", "b", "c", "d", "e"}, -3, 1),   {}              )
print( lume.slice({"a", "b", "c", "d", "e"}, 0, 1),    {"a"}           )
print( lume.slice({"a", "b", "c", "d", "e"}, 0, 0),    {}              )
print( lume.slice({"a", "b", "c", "d", "e"}, -3),      {"c", "d", "e"} )
print( lume.slice({"a", "b", "c", "d", "e"}, -3, 900), {"c", "d", "e"} )

local t = { "a", "b", "c", "d", "e" }
print( lume.first(t),    "a"           )
print( lume.first(t, 1), { "a" }       )
print( lume.first(t, 2), { "a", "b" }  )

local t = { "a", "b", "c", "d", "e" }
print( lume.last(t),     "e"           )
print( lume.last(t, 1),  { "e" }       )
print( lume.last(t, 2),  { "d", "e" }  )

print( lume.invert({}),                        {}                  )
print( lume.invert{a = "x", b = "y"},          {x = "a", y = "b"}  )
print( lume.invert{a = 1, b = 2},              {"a", "b"}          )
print( lume.invert(lume.invert{a = 1, b = 2}), {a = 1, b = 2}      )

local t = { cat = 10, dog = 20, fox = 30, owl = 40 }
print( lume.pick(t, "cat", "dog"), { cat = 10, dog = 20 } )
print( lume.pick(t, "fox", "owl"), { fox = 30, owl = 40 } )
print( lume.pick(t, "owl"), { owl = 40 } )
print( lume.pick(t), {} )

print( lume.keys({}), {} )
local t = lume.keys({ aaa = 1, bbb = 2, ccc = 3 })
table.sort(t)
print( t, {"aaa", "bbb", "ccc"} )
local t = lume.keys({ "x", "x", "x" })
print( t, {1, 2, 3} )

local t = {6, 7, 4, 5}
print( lume.clone(t) ~= t,       true         )
print( lume.clone(t),            {6, 7, 4, 5} )
print( lume.clone({x=2, y="a"}), {x=2, y="a"} )

local f = lume.fn(function(a, b) return a + b end, 10)
print( f(5),  15 )

local f = lume.once(function(a, b) return a + b end, 10)
print( f(5),  15   )
print( f(5),  nil  )

local f = lume.memoize(
function(a, b, c)
    return tostring(a) .. tostring(b) .. tostring(c)
end)
print( f("hello", nil, 15),  "hellonil15"  )
print( f("hello", nil, 15),  "hellonil15"  )
print( f(),                  "nilnilnil"   )
print( f(),                  "nilnilnil"   )
local f2 = lume.memoize(function() end)
print( f2(), nil )

local acc = 0
local a = function(x, y) acc = acc + x + y end
local b = function(x, y) acc = acc + x * y end
local fn = lume.combine(a, b)
fn(10, 20)
print( acc, 230 )
acc = 0
fn = lume.combine(nil, a, nil, b, nil)
fn(10, 20)
print( acc, 230 )
local x = false
fn = lume.combine(function() x = true end)
fn()
print( x, true )
print( type(lume.combine(nil)), "function" )
print( type(lume.combine()),    "function" )

local add = function(a, b) return a + b end
print( lume.call(),              nil )
print( lume.call(nil, 1, 2, 3),  nil )
print( lume.call(add, 1, 2),     3   )

local t, a, b, c = lume.time(function(x) return 50, 60, x end, 70)
print( type(t),    "number"      )
print( {a, b, c},  {50, 60, 70}  )

print( lume.lambda "x->x*x"(10),                 100         )
print( lume.lambda "x->x*x"(20),                 400         )
print( lume.lambda "x,y -> 2*x+y"(10,5),         25          )
print( lume.lambda "a, b -> a / b"(1, 2),        .5          )
print( lume.lambda "a -> 'hi->' .. a"("doggy"),  "hi->doggy" )
print( lume.lambda "A1,_->A1.._"("te","st"),     "test"      )
print( lume.lambda "->"(1,2,3),                  nil         )

print({ lume.color("#ff0000") },                   { 1, 0, 0, 1 }  )
print({ lume.color("#00ff00") },                   { 0, 1, 0, 1 }  )
print({ lume.color("#0000ff") },                   { 0, 0, 1, 1 }  )

local str = lume.format("a {a} in a {b}", {a = "mouse", b = "house"})
print( str, "a mouse in a house" )
print( lume.format("number {num}", {num = 13}),    "number 13"         )
print( lume.format("{missing} {keys}", {}),        "{missing} {keys}"  )
print( lume.format("A {missing} table"),           "A {missing} table" )
print( lume.format("{1} idx {2}", {"an", "test"}), "an idx test"       )
print( lume.format("bad idx {-1}", {"x"}),         "bad idx {-1}"      )
print( lume.format("empty {}", {"idx"}),           "empty {}"          )

--print(lume.split("cat   dog  pig"))
print(lume.trim("   hello world   "))

--lume.wordwrap("Hello world. This is a short string", 14)

print( type(lume.uuid()), "string" )
print( #lume.uuid(),      36       )

local t = { "a", "b", false, "c" }
local r = {}
for i, v in lume.ripairs(t) do
table.insert(r, { i, v })
end
print( r, { { 4, "c" }, { 3, false }, { 2, "b" }, { 1, "a" } })

local t = lume.chain({1, 2}):map(function(x) return x * 2 end):result()
print( t, { 2, 4 } )
print( lume.chain(10):result(), 10 )
local t = lume({1, 2}):map(function(x) return x * 2 end):result()
print( t, { 2, 4 } )


--

print(lume.round(2.3)) -- Returns 2
print(lume.round(123.4567, .1)) -- Returns 123.5

print( lume.sign(6) )

print( lume.lerp(100, 200, .5) )

local x, y = lume.vector(0, 10)


print( lume.randomchoice({true, false}) )
print( lume.weightedchoice({ ["cat"] = 10, ["dog"] = 5, ["frog"] = 0 }) )
print( lume.isarray(x) )

local t = { 1, 2, 3 }
lume.push(t, 4, 5) -- `t` becomes { 1, 2, 3, 4, 5 }

print( t[1] .. " " .. t[5] )

local t = { 1, 2, 3 }
lume.remove(t, 2)

print( lume.count(t) )

lume.clear(t)

print( lume.count(t) )

print(lume.time(function(x) return x end, "hello")) -- Returns 0, "hello"

local f = lume.lambda "x,y -> 2*x+y"
print(f(10, 5))

--lume.serialize({a = "test", b = {1, 2, 3}})

local t = lume.deserialize("{1, 2, 3}") -- Returns {1, 2, 3}

print(t[1])

-- lume.split("One two three")

for i, v in lume.ripairs({ "a", "b", "c" }) do
    print(i .. "->" .. v)
end

lume.dostring("print('Hellox!')") -- Prints "Hellox!"

print(lume.trim("  Hello  "))

print(lume.trim("   hello world   "))

print( lume.chain({1, 2, 3, 4})
:filter(function(x) return x % 2 == 0 end)
:map(function(x) return -x end)
:result()[1] )

lume({1, 2, 3}):each(print)