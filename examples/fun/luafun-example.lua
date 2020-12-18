-- File    : luafun-example.lua
-- Purpose : Brief demonstration of Lua Fun on Luerl.
-- See     : ./luafun.erl


local fun = require('fun')

for _k, a in fun.range(3) do print(a) end

print(fun.sum(fun.filter(function(x) return x % 16 == 0 end, fun.range(10000))))

for k, v in pairs(require("fun")) do _G[k] = v end

each(print, range(5))

each(print, take(5, tabulate(math.sin)))

each(print, enumerate(zip({"one", "two", "three", "four", "five"},
    {"a", "b", "c", "d", "e"})))

lines_to_grep = {
    [[Emily]],
    [[Chloe]],
    [[Megan]],
    [[Jessica]],
    [[Emma]],
    [[Sarah]],
    [[Elizabeth]],
    [[Sophie]],
    [[Olivia]],
    [[Lauren]]
}

each(print, grep("Em", lines_to_grep))

each(print, take(10, cycle(chain(
    {enumerate({"a", "b", "c"})},
    {"one", "two", "three"}))
))

--------------------------------------------------------------------------------
-- iter
--------------------------------------------------------------------------------

--
-- Arrays
--

for _it, a in iter({1, 2, 3}) do print(a) end
--[[test
1
2
3
--test]]

for _it, a in iter(iter(iter({1, 2, 3}))) do print(a) end
--[[test
1
2
3
--test]]

for _it, a in wrap(wrap(iter({1, 2, 3}))) do print(a) end
--[[test
1
2
3
--test]]

for _it, a in wrap(wrap(ipairs({1, 2, 3}))) do print(a) end
--[[test
1
2
3
--test]]

for _it, a in iter({}) do print(a) end
--[[test
--test]]

for _it, a in iter(iter(iter({}))) do print(a) end
--[[test
--test]]

for _it, a in wrap(wrap(iter({}))) do print(a) end
--[[test
--test]]

for _it, a in wrap(wrap(ipairs({}))) do print(a) end
--[[test
--test]]

-- Check that ``iter`` for arrays is equivalent to ``ipairs``
local t = {1, 2, 3}
gen1, param1, state1 = iter(t):unwrap()
gen2, param2, state2 = ipairs(t) 
print(gen1 == gen2, param1 == param2, state1 == state2)
--[[test
true true true
--test]]

-- Test that ``wrap`` do nothing for wrapped iterators
gen1, param1, state1 = iter({1, 2, 3})
gen2, param2, state2 = wrap(gen1, param1, state1):unwrap()
print(gen1 == gen2, param1 == param2, state1 == state2)
--[[test
true true true
--test]]

--
-- Maps
--

local t = {}
for _it, k, v in iter({ a = 1, b = 2, c = 3}) do t[#t + 1] = k end
table.sort(t)
for _it, v in iter(t) do print(v) end
--[[test
a
b
c
--test]]

local t = {}
for _it, k, v in iter(iter(iter({ a = 1, b = 2, c = 3}))) do t[#t + 1] = k end
table.sort(t)
for _it, v in iter(t) do print(v) end
--[[test
a
b
c
--test]]

for _it, k, v in iter({}) do print(k, v) end
--[[test
--test]]

for _it, k, v in iter(iter(iter({}))) do print(k, v) end
--[[test
--test]]

--
-- String
--

for _it, a in iter("abcde") do print(a) end
--[[test
a
b
c
d
e
--test]]

for _it, a in iter(iter(iter("abcde"))) do print(a) end
--[[test
a
b
c
d
e
--test]]

for _it, a in iter("") do print(a) end
--[[test
--test]]

for _it, a in iter(iter(iter(""))) do print(a) end
--[[test
--test]]

--
-- Custom generators
--

local function mypairs_gen(max, state)
    if (state >= max) then
            return nil
        end
        return state + 1, state + 1
end

local function mypairs(max)
    return mypairs_gen, max, 0
end

for _it, a in iter(mypairs(10)) do print(a) end
--[[test
1
2
3
4
5
6
7
8
9
10
--test]]

--
-- Invalid values
--

--for _it, a in iter(1) do print(a) end
--[[test
error: object 1 of type "number" is not iterable
--test]]

--for _it, a in iter(1, 2, 3, 4, 5, 6, 7) do print(a) end
--[[test
error: object 1 of type "number" is not iterable
--test]]

--------------------------------------------------------------------------------
-- each
--------------------------------------------------------------------------------

each(print, {1, 2, 3})
--[[test
1
2
3
--test]]

each(print, iter({1, 2, 3}))
--[[test
1
2
3
--test]]

each(print, {})
--[[test
--test]]


each(print, iter({}))
--[[test
--test]]

local keys, vals = {}, {}
each(function(k, v)
    keys[#keys + 1] = k
    vals[#vals + 1] = v
end, { a = 1, b = 2, c = 3})
table.sort(keys)
table.sort(vals)
each(print, keys)
each(print, vals)
--[[test
a
b
c
1
2
3
--test]]

each(print, "abc")
--[[test
a
b
c
--test]]

each(print, iter("abc"))
--[[test
a
b
c
--test]]

print(for_each == each) -- an alias
--[[test
true
--test]]

print(foreach == each) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- totable
--------------------------------------------------------------------------------

local tab = totable(range(5))
print(type(tab), #tab)
each(print, tab)
--[[test
table 5
1
2
3
4
5
--test]]

local tab = totable(range(0))
print(type(tab), #tab)
--[[test
table 0
--test]]

local tab = totable("abcdef")
print(type(tab), #tab)
each(print, tab)
--[[test
table 6
a
b
c
d
e
f
--test]]

local unpack = rawget(table, "unpack") or unpack
local tab = totable({ 'a', {'b', 'c'}, {'d', 'e', 'f'}})
print(type(tab), #tab)
each(print, tab[1])
each(print, map(unpack, drop(1, tab)))
--[[test
table 3
a
b c
d e f
--test]]

--------------------------------------------------------------------------------
-- tomap
--------------------------------------------------------------------------------

local tab = tomap(zip(range(1, 7), 'abcdef'))
print(type(tab), #tab)
each(print, iter(tab))
--[[test
table 6
a
b
c
d
e
f
--test]]

local tab = tomap({a = 1, b = 2, c = 3})
print(type(tab), #tab)
local t = {}
for _it, k, v in iter(tab) do t[v] = k end
table.sort(t)
for k, v in ipairs(t) do print(k, v) end
--[[test
table 0
1 a
2 b
3 c
--test]]

local tab = tomap(enumerate("abcdef"))
print(type(tab), #tab)
each(print, tab)
--[[test
table 6
a
b
c
d
e
f
--test]]
--------------------------------------------------------------------------------
-- zip
--------------------------------------------------------------------------------

print(zip({"a", "b", "c", "d"}, {"one", "two", "three"}))
--[[test
a one
b two
c three
--test]]

print(zip())
--[[test
--test]]

--print(zip(range(0)))
--[[test
error: invalid iterator
--test]]

--print(zip(range(0), range(0)))
--[[test
error: invalid iterator
--test]]

print(nth(10, zip(range(1, 100, 3), range(1, 100, 5), range(1, 100, 7))))
--[[test
28 46 64
--test]]

print(zip(partition(function(x) return x > 7 end, range(1, 15, 1))))
--[[test
8 1
9 2
10 3
11 4
12 5
13 6
14 7
--test]]

--------------------------------------------------------------------------------
-- cycle
--------------------------------------------------------------------------------

print(take(15, cycle({"a", "b", "c", "d", "e"})))
--[[test
a
b
c
d
e
a
b
c
d
e
a
b
c
d
e
--test]]


print(take(15, cycle(range(5))))
--[[test
1
2
3
4
5
1
2
3
4
5
1
2
3
4
5
--test]]

print(take(15, cycle(zip(range(5), {"a", "b", "c", "d", "e"}))))
--[[test
1 a
2 b
3 c
4 d
5 e
1 a
2 b
3 c
4 d
5 e
1 a
2 b
3 c
4 d
5 e
--test]]

--------------------------------------------------------------------------------
-- chain
--------------------------------------------------------------------------------

print(chain(range(2)))
--[[test
1
2
--test]]

print(chain(range(2), {"a", "b", "c"}, {"one", "two", "three"}))
--[[test
1
2
a
b
c
one
two
three
--test]]

print(take(15, cycle(chain(enumerate({"a", "b", "c"}),
    {"one", "two", "three"}))))
--[[test
1 a
2 b
3 c
one
two
three
1 a
2 b
3 c
one
two
three
1 a
2 b
3 c
--test]]

local tab = {}
local keys = {}
for _it, k, v in chain({ a = 11, b = 12, c = 13}, { d = 21, e = 22 }) do
    tab[k] = v
    table.insert(keys, k)
end
table.sort(keys)
for _, key in ipairs(keys) do print(key, tab[key]) end
--[[test
a 11
b 12
c 13
d 21
e 22
--test]]

--print(chain(range(0), range(0), range(0)))
--[[test
error: invalid iterator
--test]]

--print(chain(range(0), range(1), range(0)))
--[[test
error: invalid iterator
--test]]
--------------------------------------------------------------------------------
-- filter
--------------------------------------------------------------------------------

print(filter(function(x) return x % 3 == 0 end, range(10)))
--[[test
3
6
9
--test]]

print(filter(function(x) return x % 3 == 0 end, range(0)))
--[[test
--test]]


print(take(5, filter(function(i, x) return i % 3 == 0 end,
    enumerate(duplicate('x')))))
--[[test
3 x
6 x
9 x
12 x
15 x
--test]]

function filter_fun(a, b, c)
    if a % 16 == 0 then
        return true
    else
        return false
    end
end

function test3(a, b, c)
    return a, c, b
end

n = 50
print(filter(filter_fun, map(test3, zip(range(0, n, 1),
     range(0, n, 2), range(0, n, 3)))))
--[[test
0 0 0
16 48 32
--test]]

print(remove_if == filter) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- grep
--------------------------------------------------------------------------------

lines_to_grep = {
    [[Lorem ipsum dolor sit amet, consectetur adipisicing elit, ]],
    [[sed do eiusmod tempor incididunt ut labore et dolore magna ]],
    [[aliqua. Ut enim ad minim veniam, quis nostrud exercitation ]],
    [[ullamco laboris nisi ut aliquip ex ea commodo consequat.]],
    [[Duis aute irure dolor in reprehenderit in voluptate velit ]],
    [[esse cillum dolore eu fugiat nulla pariatur. Excepteur sint ]],
    [[occaecat cupidatat non proident, sunt in culpa qui officia ]],
    [[deserunt mollit anim id est laborum.]]
}

print(grep("lab", lines_to_grep))
--[[test
sed do eiusmod tempor incididunt ut labore et dolore magna 
ullamco laboris nisi ut aliquip ex ea commodo consequat.
deserunt mollit anim id est laborum.
--test]]

lines_to_grep = {
    [[Emily]],
    [[Chloe]],
    [[Megan]],
    [[Jessica]],
    [[Emma]],
    [[Sarah]],
    [[Elizabeth]],
    [[Sophie]],
    [[Olivia]],
    [[Lauren]]
}

print(grep("^Em", lines_to_grep))
--[[test
Emily
Emma
--test]]

--------------------------------------------------------------------------------
-- partition
--------------------------------------------------------------------------------

print(zip(partition(function(i, x) return i % 3 == 0 end, range(10))))
--[[test
3 1
6 2
9 4
--test]]
--------------------------------------------------------------------------------
-- range
--------------------------------------------------------------------------------

print(range(0))
print('--')
for i=1,0 do print(i) end
--[[test
--
--test]]

print(range(0, 0))
print('--')
for i=0,0 do print(i) end
--[[test
0
--
0
--test]]

print(range(5))
print('--')
for i=1,5 do print(i) end
--[[test
1
2
3
4
5
--
1
2
3
4
5
--test]]

print(range(0, 5))
print('--')
for i=0,5 do print(i) end
--[[test
0
1
2
3
4
5
--
0
1
2
3
4
5
--test]]

print(range(0, 5, 1))
print('--')
for i=0,5,1 do print(i) end
--[[test
0
1
2
3
4
5
--
0
1
2
3
4
5
--test]]

print(range(0, 10, 2))
print('--')
for i=0,10,2 do print(i) end
--[[test
0
2
4
6
8
10
--
0
2
4
6
8
10
--test]]

print(range(-5))
print('--')
for i=-1,-5,-1 do print(i) end
--[[test
-1
-2
-3
-4
-5
--
-1
-2
-3
-4
-5
--test]]

print(range(0, -5, 1))
print('--')
for i=0,-5,1 do print(i) end
--[[test
--
--test]]

print(range(0, -5, -1))
print('--')
for i=0,-5,-1 do print(i) end
--[[test
0
-1
-2
-3
-4
-5
--
0
-1
-2
-3
-4
-5
--test]]

print(range(0, -10, -2))
print('--')
for i=0,-10,-2 do print(i) end
--[[test
0
-2
-4
-6
-8
-10
--
0
-2
-4
-6
-8
-10
--test]]

print(range(1.2, 1.6, 0.1))
--[[test
1.2
1.3
1.4
1.5
--test]]

-- Invalid step
--print(range(0, 5, 0))
--[[test
error: step must not be zero
--test]]

--------------------------------------------------------------------------------
-- duplicate
--------------------------------------------------------------------------------

print(take(5, duplicate(48)))
--[[test
48
48
48
48
48
--test]]

print(take(5, duplicate(1,2,3,4,5)))
--[[test
1 2 3 4 5
1 2 3 4 5
1 2 3 4 5
1 2 3 4 5
1 2 3 4 5
--test]]

print(xrepeat == duplicate) -- an alias
--[[test
true
--test]]

print(replicate == duplicate) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- tabulate
--------------------------------------------------------------------------------

print(take(5, tabulate(function(x) return 2 * x end)))
--[[test
0
2
4
6
8
--test]]

--------------------------------------------------------------------------------
-- zeros
--------------------------------------------------------------------------------

print(take(5, zeros()))
--[[test
0
0
0
0
0
--test]]

--------------------------------------------------------------------------------
-- ones
--------------------------------------------------------------------------------

print(take(5, ones()))
--[[test
1
1
1
1
1
--test]]

--------------------------------------------------------------------------------
-- rands
--------------------------------------------------------------------------------

print(all(function(x) return x >= 0 and x < 1 end, take(5, rands())))
--[[test
true
--test]]

--print(take(5, rands(0)))
--[[test
error: empty interval
--test]]

print(all(function(x) return math.floor(x) == x end, take(5, rands(10))))
--[[test
true
--test]]

print(all(function(x) return math.floor(x) == x end, take(5, rands(1024))))
--[[test
true
--test]]

print(take(5, rands(0, 1)))
--[[test
0
0
0
0
0
--test]]

print(take(5, rands(5, 6)))
--[[test
5
5
5
5
5
--test]]

print(all(function(x) return x >= 10 and x < 20 end, take(20, rands(10, 20))))
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- index
--------------------------------------------------------------------------------

print(index(2, range(5)))
--[[test
2
--test]]

print(index(10, range(5)))
--[[test
nil
--test]]

print(index(2, range(0)))
--[[test
nil
--test]]

print(index("b", {"a", "b", "c", "d", "e"}))
--[[test
2
--test]]

print(index(1, enumerate({"a", "b", "c", "d", "e"})))
--[[test
1
--test]]

print(index("b", "abcdef"))
--[[test
2
--test]]

print(index_of == index) -- an alias
--[[test
true
--test]]

print(elem_index == index) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- indexes
--------------------------------------------------------------------------------

print(indexes("a", {"a", "b", "c", "d", "e", "a", "b", "c", "d", "a", "a"}))
--[[test
1
6
10
11
--test]]

print(indexes("f", {"a", "b", "c", "d", "e", "a", "b", "c", "d", "a", "a"}))
--[[test
--test]]

print(indexes("f", {}))
--[[test
--test]]

print(indexes(1, enumerate({"a", "b", "c", "d", "e"})))
--[[test
1
--test]]

print(indices == indexes) -- an alias
--[[test
true
--test]]

print(elem_indexes == indexes) -- an alias
--[[test
true
--test]]

print(elem_indices == indexes) -- an alias
--[[test
true
--test]]

--
-- All these functions are fully covered by Lua tests.
-- This test just checks that all functions were defined correctly.
--

print(op == operator) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- Comparison operators
--------------------------------------------------------------------------------

local comparators = { 'le', 'lt', 'eq', 'ne', 'ge', 'gt' }
for _k, op in iter(comparators) do
    print('op', op)
    print('==')
    print('num:')
    print(operator[op](0, 1))
    print(operator[op](1, 0))
    print(operator[op](0, 0))
    print('str:')
    print(operator[op]("abc", "cde"))
    print(operator[op]("cde", "abc"))
    print(operator[op]("abc", "abc"))
    print('')
end
--[[test
op le
==
num:
true
false
true
str:
true
false
true

op lt
==
num:
true
false
false
str:
true
false
false

op eq
==
num:
false
false
true
str:
false
false
true

op ne
==
num:
true
true
false
str:
true
true
false

op ge
==
num:
false
true
true
str:
false
true
true

op gt
==
num:
false
true
false
str:
false
true
false

--test]]

--------------------------------------------------------------------------------
-- Arithmetic operators
--------------------------------------------------------------------------------

print(operator.add(-1.0, 1.0))
print(operator.add(0, 0))
print(operator.add(12, 2))
--[[test
0
0
14
--test]]

print(operator.div(10, 2))
print(operator.div(10, 3))
print(operator.div(-10, 3))
--[[test
5
3.3333333333333
-3.3333333333333
--test]]

print(operator.floordiv(10, 3))
print(operator.floordiv(11, 3))
print(operator.floordiv(12, 3))
print(operator.floordiv(-10, 3))
print(operator.floordiv(-11, 3))
print(operator.floordiv(-12, 3))
--[[test
3
3
4
-4
-4
-4
--test]]

print(operator.intdiv(10, 3))
print(operator.intdiv(11, 3))
print(operator.intdiv(12, 3))
print(operator.intdiv(-10, 3))
print(operator.intdiv(-11, 3))
print(operator.intdiv(-12, 3))
--[[test
3
3
4
-3
-3
-4
--test]]

print(operator.truediv(10, 3))
print(operator.truediv(11, 3))
print(operator.truediv(12, 3))
print(operator.truediv(-10, 3))
print(operator.truediv(-11, 3))
print(operator.truediv(-12, 3))
--[[test
3.3333333333333
3.6666666666667
4
-3.3333333333333
-3.6666666666667
-4
--test]]

print(operator.mod(10, 2))
print(operator.mod(10, 3))
print(operator.mod(-10, 3))
--[[test
0
1
2
--test]]

print(operator.mul(10, 0.1))
print(operator.mul(0, 0))
print(operator.mul(-1, -1))
--[[test
1
0
1
--test]]

print(operator.neq(1))
print(operator.neq(0) == 0)
print(operator.neq(-0) == 0)
print(operator.neq(-1))
--[[test
-1
true
true
1
--test]]

print(operator.unm(1))
print(operator.unm(0) == 0)
print(operator.unm(-0) == 0)
print(operator.unm(-1))
--[[test
-1
true
true
1
--test]]

print(operator.pow(2, 3))
print(operator.pow(0, 10))
print(operator.pow(2, 0))
--[[test
8
0
1
--test]]

print(operator.sub(2, 3))
print(operator.sub(0, 10))
print(operator.sub(2, 2))
--[[test
-1
-10
0
--test]]

--------------------------------------------------------------------------------
-- String operators
--------------------------------------------------------------------------------

print(operator.concat("aa", "bb"))
print(operator.concat("aa", ""))
print(operator.concat("", "bb"))
--[[test
aabb
aa
bb
--test]]

print(operator.len(""))
print(operator.len("ab"))
print(operator.len("abcd"))
--[[test
0
2
4
--test]]

print(operator.length(""))
print(operator.length("ab"))
print(operator.length("abcd"))
--[[test
0
2
4
--test]]

----------------------------------------------------------------------------
-- Logical operators
----------------------------------------------------------------------------

print(operator.land(true, true))
print(operator.land(true, false))
print(operator.land(false, true))
print(operator.land(false, false))
print(operator.land(1, 0))
print(operator.land(0, 1))
print(operator.land(1, 1))
print(operator.land(0, 0))
--[[test
true
false
false
false
0
1
1
0
--test]]

print(operator.lor(true, true))
print(operator.lor(true, false))
print(operator.lor(false, true))
print(operator.lor(false, false))
print(operator.lor(1, 0))
print(operator.lor(0, 1))
print(operator.lor(1, 1))
print(operator.lor(0, 0))
--[[test
true
true
true
false
1
0
1
0
--test]]

print(operator.lnot(true))
print(operator.lnot(false))
print(operator.lor(1))
print(operator.lor(0))
--[[test
false
true
1
0
--test]]

print(operator.truth(true))
print(operator.truth(false))
print(operator.truth(1))
print(operator.truth(0))
print(operator.truth(nil))
print(operator.truth(""))
print(operator.truth({}))
--[[test
true
false
true
true
false
true
true
--test]]
--------------------------------------------------------------------------------
-- foldl
--------------------------------------------------------------------------------

print(foldl(function(acc, x) return acc + x end, 0, range(5)))
--[[test
15
--test]]

print(foldl(operator.add, 0, range(5)))
--[[test
15
--test]]

print(foldl(function(acc, x, y) return acc + x * y; end, 0,
    zip(range(1, 5), {4, 3, 2, 1})))
--[[test
20
--test]]

print(reduce == foldl) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- length
--------------------------------------------------------------------------------

print(length({"a", "b", "c", "d", "e"}))
--[[test
5
--test]]

print(length({}))
--[[test
0
--test]]

print(length(range(0)))
--[[test
0
--test]]

--------------------------------------------------------------------------------
-- is_null
--------------------------------------------------------------------------------

print(is_null({"a", "b", "c", "d", "e"}))
--[[test
false
--test]]

print(is_null({}))
--[[test
true
--test]]

print(is_null(range(0)))
--[[test
true
--test]]

local gen, init, state = range(5)
print(is_null(gen, init, state))
print(gen, init, state)
--[[test
false
1
2
3
4
5
--test]]

--------------------------------------------------------------------------------
-- is_prefix_of
--------------------------------------------------------------------------------

print(is_prefix_of({"a"}, {"a", "b", "c"}))
--[[test
true
--test]]

print(is_prefix_of({}, {"a", "b", "c"}))
--[[test
true
--test]]

print(is_prefix_of({}, {}))
--[[test
true
--test]]

print(is_prefix_of({"a"}, {}))
--[[test
false
--test]]

print(is_prefix_of(range(5), range(6)))
--[[test
true
--test]]

print(is_prefix_of(range(6), range(5)))
--[[test
false
--test]]

--------------------------------------------------------------------------------
-- all
--------------------------------------------------------------------------------

print(all(function(x) return x end, {true, true, true, true}))
--[[test
true
--test]]

print(all(function(x) return x end, {true, true, true, false}))
--[[test
false
--test]]

print(all(function(x) return x end, {}))
--[[test
true
--test]]

print(every == all) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- any
--------------------------------------------------------------------------------

print(any(function(x) return x end, {false, false, false, false}))
--[[test
false
--test]]

print(any(function(x) return x end, {false, false, false, true}))
--[[test
true
--test]]

print(any(function(x) return x end, {}))
--[[test
false
--test]]

print(some == any) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- sum
--------------------------------------------------------------------------------

print(sum(range(1, 5)))
--[[test
15
--test]]

print(sum(range(1, 5, 0.5)))
--[[test
27
--test]]

print(sum(range(0)))
--[[test
0
--test]]

--------------------------------------------------------------------------------
-- product
--------------------------------------------------------------------------------

print(product(range(1, 5)))
--[[test
120
--test]]

print(product(range(1, 5, 0.5)))
--[[test
7087.5
--test]]

print(product(range(0)))
--[[test
1
--test]]


--------------------------------------------------------------------------------
-- min
--------------------------------------------------------------------------------

print(min(range(1, 10, 1)))
--[[test
1
--test]]

print(min({"f", "d", "c", "d", "e"}))
--[[test
c
--test]]

--print(min({}))
--[[test
error: min: iterator is empty
--test]]

print(minimum == min) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- min_by
--------------------------------------------------------------------------------

function min_cmp(a, b) if -a < -b then return a else return b end end
--[[test
--test]]

print(min_by(min_cmp, range(1, 10, 1)))
--[[test
10
--test]]

--print(min_by(min_cmp, {}))
--[[test
error: min: iterator is empty
--test]]

print(minimum_by == min_by) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- max
--------------------------------------------------------------------------------

print(max(range(1, 10, 1)))
--[[test
10
--test]]

print(max({"f", "d", "c", "d", "e"}))
--[[test
f
--test]]

--print(max({}))
--[[test
error: max: iterator is empty
--test]]

print(maximum == max) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- max_by
--------------------------------------------------------------------------------

function max_cmp(a, b) if -a > -b then return a else return b end end
--[[test
--test]]

print(max_by(max_cmp, range(1, 10, 1)))
--[[test
1
--test]]

--print(max_by(max_cmp, {}))
--[[test
error: max: iterator is empty
--test]]

print(maximum_by == maximum_by) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- nth
--------------------------------------------------------------------------------

print(nth(2, range(5)))
--[[test
2
--test]]

print(nth(10, range(5)))
--[[test
nil
--test]]

print(nth(2, range(0)))
--[[test
nil
--test]]

print(nth(2, {"a", "b", "c", "d", "e"}))
--[[test
b
--test]]

print(nth(2, enumerate({"a", "b", "c", "d", "e"})))
--[[test
2 b
--test]]

print(nth(1, "abcdef"))
--[[test
a
--test]]

print(nth(2, "abcdef"))
--[[test
b
--test]]

print(nth(6, "abcdef"))
--[[test
f
--test]]

--print(nth(0, "abcdef"))
--[[test
error: invalid first argument to nth
--test]]

print(nth(7, "abcdef"))
--[[test
nil
--test]]

--------------------------------------------------------------------------------
-- head
--------------------------------------------------------------------------------

print(head({"a", "b", "c", "d", "e"}))
--[[test
a
--test]]

--print(head({}))
--[[test
error: head: iterator is empty
--test]]

--print(head(range(0)))
--[[test
error: head: iterator is empty
--test]]

print(head(enumerate({"a", "b"})))
--[[test
1 a
--test]]

print(car == head) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- tail
--------------------------------------------------------------------------------

print(tail({"a", "b", "c", "d", "e"}))
--[[test
b
c
d
e
--test]]

print(tail({}))
--[[test
--test]]

print(tail(range(0)))
--[[test
--test]]

print(tail(enumerate({"a", "b"})))
--[[test
2 b
--test]]

print(cdr == tail) -- an alias
--[[test
true
--test]]


--------------------------------------------------------------------------------
-- take_n
--------------------------------------------------------------------------------

print(take_n(0, duplicate(48)))
--[[test
--test]]

print(take_n(5, range(0)))
--[[test
--test]]

print(take_n(1, duplicate(48)))
--[[test
48
--test]]

print(take_n(5, duplicate(48)))
--[[test
48
48
48
48
48
--test]]

print(take_n(5, enumerate(duplicate('x'))))
--[[test
1 x
2 x
3 x
4 x
5 x
--test]]

--------------------------------------------------------------------------------
-- take_while
--------------------------------------------------------------------------------

print(take_while(function(x) return x < 5 end, range(10)))
--[[test
1
2
3
4
--test]]

print(take_while(function(x) return x < 5 end, range(0)))
--[[test
--test]]

print(take_while(function(x) return x > 100 end, range(10)))
--[[test
--test]]

print(take_while(function(i, a) return i ~=a end, enumerate({5, 2, 1, 3, 4})))
--[[test
1 5
--test]]

--------------------------------------------------------------------------------
-- take
--------------------------------------------------------------------------------

print(take(function(x) return x < 5 end, range(10)))
--[[test
1
2
3
4
--test]]

print(take(5, duplicate(48)))
--[[test
48
48
48
48
48
--test]]

--------------------------------------------------------------------------------
-- drop_n
--------------------------------------------------------------------------------

print(drop_n(5, range(10)))
--[[test
6
7
8
9
10
--test]]

print(drop_n(0, range(5)))
--[[test
1
2
3
4
5
--test]]

print(drop_n(5, range(0)))
--[[test
--test]]

print(drop_n(2, enumerate({'a', 'b', 'c', 'd', 'e'})))
--[[test
3 c
4 d
5 e
--test]]

--------------------------------------------------------------------------------
-- drop_while
--------------------------------------------------------------------------------

print(drop_while(function(x) return x < 5 end, range(10)))
--[[test
5
6
7
8
9
10
--test]]

print(drop_while(function(x) return x < 5 end, range(0)))
--[[test
--test]]

print(drop_while(function(x) return x > 100 end, range(10)))
--[[test
1
2
3
4
5
6
7
8
9
10
--test]]

print(drop_while(function(i, a) return i ~=a end, enumerate({5, 2, 1, 3, 4})))
--[[test
2 2
3 1
4 3
5 4
--test]]

print(drop_while(function(i, a) return i ~=a end,
    zip({1, 2, 3, 4, 5}, {5, 4, 3, 2, 1})))
--[[test
3 3
4 2
5 1
--test]]

--------------------------------------------------------------------------------
-- drop
--------------------------------------------------------------------------------

print(drop(5, range(10)))
--[[test
6
7
8
9
10
--test]]

print(drop(function(x) return x < 5 end, range(10)))
--[[test
5
6
7
8
9
10
--test]]


--------------------------------------------------------------------------------
-- span
--------------------------------------------------------------------------------

print(zip(span(function(x) return x < 5 end, range(10))))
--[[test
1 5
2 6
3 7
4 8
--test]]

print(zip(span(5, range(10))))
--[[test
1 6
2 7
3 8
4 9
5 10
--test]]

print(zip(span(function(x) return x < 5 end, range(0))))
--[[test
--test]]

print(zip(span(function(x) return x < 5 end, range(5))))
--[[test
1 5
--test]]

print(split == span) -- an alias
--[[test
true
--test]]

print(split_at == span) -- an alias
--[[test
true
--test]]

--------------------------------------------------------------------------------
-- map
--------------------------------------------------------------------------------

fun = function(...) return 'map', ... end

print(map(fun, range(0)))
--[[test
--test]]


print(map(fun, range(4)))
--[[test
map 1
map 2
map 3
map 4
--test]]

print(map(fun, enumerate({"a", "b", "c", "d", "e"})))
--[[test
map 1 a
map 2 b
map 3 c
map 4 d
map 5 e
--test]]

print(map(function(x) return 2 * x end, range(4)))
--[[test
2
4
6
8
--test]]

fun = nil
--[[test
--test]]

--------------------------------------------------------------------------------
-- enumerate
--------------------------------------------------------------------------------

print(enumerate({"a", "b", "c", "d", "e"}))
--[[test
1 a
2 b
3 c
4 d
5 e
--test]]

print(enumerate(enumerate(enumerate({"a", "b", "c", "d", "e"}))))
--[[test
1 1 1 a
2 2 2 b
3 3 3 c
4 4 4 d
5 5 5 e
--test]]

print(enumerate(zip({"one", "two", "three", "four", "five"},
    {"a", "b", "c", "d", "e"})))
--[[test
1 one a
2 two b
3 three c
4 four d
5 five e
--test]]

--------------------------------------------------------------------------------
-- intersperse
--------------------------------------------------------------------------------

print(intersperse("x", {}))

print(intersperse("x", {"a", "b", "c", "d", "e"}))
--[[test
a
x
b
x
c
x
d
x
e
x
--test]]

print(intersperse("x", {"a", "b", "c", "d", "e", "f"}))
--[[test
a
x
b
x
c
x
d
x
e
x
f
x
--test]]

print('Hello Luerl!')