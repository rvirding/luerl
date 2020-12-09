-- File    : moses-example.lua
-- Purpose : Brief demonstration of moses on Luerl.
-- See     : ./moses.erl


local M = require("moses")

M.clear({1,2,'hello',true})

M.each({4,2,1},print)

M.each({one = 1, two = 2, three = 3},print)

--t = {'a','b','c'}
--M.each(t,function(v,i)
--  t[i] = v:rep(2)
--  print(t[i])
--end)

-- M.eachi({4,2,1},print)

--local t = {a = 1, b = 2, [0] = 1, [-1] = 6, 3, x = 4, 5}
--M.eachi(t,print)

local t = {4,5,6}
print(M.at(t,1,3))

local t = {a = 4, bb = true, ccc = false}
print(M.at(t,'a', 'ccc')[1]) -- => "{4, false}"

M.count({1,1,2,3,3,3,2,4,3,2},1) -- => 2
M.count({1,1,2,3,3,3,2,4,3,2},2) -- => 3
M.count({1,1,2,3,3,3,2,4,3,2},3) -- => 4
M.count({false, false, true},false) -- => 2
M.count({false, false, true},true) -- => 1

print(M.count({1,1,2,3,3})) -- => 5


print(M.countf({1,2,3,4,5,6}, function(v)
    return v%2==0
end)) -- => 3

print(M.countf({print, pairs, os, assert, ipairs}, function(v)
    return type(v)=='function'
end)) -- => 4

--M.allEqual({1,1,1,1,1}, comp) -- => true
--M.allEqual({1,1,2,1,1}, comp) -- => false

--local t1 = {1, 2, {3}}
--local t2 = {1, 2, {3}}
--M.allEqual({t1, t2}) -- => true

--local t1 = {x = 1, y = 0}
--local t2 = {x = 1, y = 0}
--local t3 = {x = 1, y = 2}
--local t4 = {x = 1, y = 2}
--local function compx(a, b) return a.x == b.x end
--local function compy(a, b) return a.y == b.y end

--M.allEqual({t1, t2}, compx) -- => true
--M.allEqual({t1, t2}, compy) -- => true
--M.allEqual({t3, t4}, compx) -- => true
--M.allEqual({t3, t4}, compy) -- => true
--M.allEqual({t1, t2, t3, t4}, compx) -- => true
--M.allEqual({t1, t2, t3, t4}, compy) -- => false

local t = {'a','b','c'}
for v in M.cycle(t, 2) do
  print(v)
end


local t = {x = 1, y = 2, z = 3}
for v in M.cycle(t) do
  print(v)
end


print(M.map({1,2,3},function(v) 
    return v+10 
end))


M.map({a = 1, b = 2},function(v, k) 
    return k..v 
end)

--M.map({a = 1, b = 2},function(v, k) 
--    return k..k, v*2 
--end)

M.map({1,2,3},function(v) 
    return v+10 
end)

M.map({a = 1, 2, 3, 4, 5},function(v, k) 
    return k..v 
end)

local function add(a,b) return a+b end
M.reduce({1,2,3,4},add)

local function concat(a,b) return a..b end	
M.reduce({'a','b','c','d'},concat)

--local words = {'Lua', 'Programming', 'Language'}
--M.best(words, function(a,b) return #a > #b end) -- => 'Programming'
--M.best(words, function(a,b) return #a < #b end) -- => 'Lua'

local val = {-1, 8, 0, -6, 3, -1, 7, 1, -9}

local function add(a,b) return a+b end

-- predicate for negative values
local function neg(v) return v<=0 end

-- predicate for positive values
local function pos(v) return v>=0 end

--M.reduceBy(val, add, neg) -- => -17
--M.reduceBy(val, add, pos) -- => 19

--M.reduceBy(val, add, neg, 17) -- => 0
--M.reduceBy(val, add, pos, -19) -- => 0

local initial_state = 256
local function div(a,b) return a/b end
M.reduceRight({1,2,4,16},div,initial_state)


local function concat(a,b) return a..b end
M.mapReduce({'a','b','c'},concat) -- => "{'a', 'ab', 'abc'}"


local function concat(a,b) return a..b end
M.mapReduceRight({'a','b','c'},concat) -- => "{'c', 'cb', 'cba'}"


M.include({6,8,10,16,29},16) -- => true
M.include({6,8,10,16,29},1) -- => false

local complex_table = {18,{2,{3}}}
local collection = {6,{18,{2,6}},10,{18,{2,{3}}},29}
M.include(collection, complex_table) -- => true

local function isUpper(v) return v:upper()== v end
M.include({'a','B','c'},isUpper) -- => true

M.detect({6,8,10,16},8) -- => 2
M.detect({nil,true,0,true,true},false) -- => nil

local complex_table = {18,{2,6}}
local collection = {6,{18,{2,6}},10,{18,{2,{3}}},29}
M.detect(collection, complex_table) -- => 2

local function isUpper(v)
    return v:upper()==v
end
M.detect({'a','B','c'},isUpper) -- => 2

local items = {
    {height = 10, weight = 8, price = 500},
    {height = 10, weight = 15, price = 700},
    {height = 15, weight = 15, price = 3000},
    {height = 10, weight = 8, price = 3000},
}
M.where(items, {height = 10}) -- => {items[1], items[2], items[4]}
M.where(items, {weight = 15}) -- => {items[2], items[3]}
M.where(items, {prince = 3000}) -- => {items[3], items[4]}
M.where(items, {height = 10, weight = 15, prince = 700}) -- => {items[2]}

local a = {a = 1, b = 2, c = 3}
local b = {a = 2, b = 3, d = 4}
local c = {a = 3, b = 4, e = 5}
M.findWhere({a, b, c}, {a = 3, b = 4})

local function isEven(v) return v%2==0 end
local function isOdd(v) return v%2~=0 end

M.select({1,2,3,4,5,6,7}, isEven) -- => "{2,4,6}"
M.select({1,2,3,4,5,6,7}, isOdd) -- => "{1,3,5,7}"

local function isEven(v) return v%2==0 end
local function isOdd(v) return v%2~=0 end

M.reject({1,2,3,4,5,6,7}, isEven) -- => "{1,3,5,7}"
M.reject({1,2,3,4,5,6,7}, isOdd) -- => "{2,4,6}"

local function isEven(v) return v%2==0 end
M.all({2,4,6}, isEven) -- => true

M.invoke({'a','bea','cdhza'},string.len) -- => "{1,3,5}"

local a, b, c, d = {id = 'a'}, {id = 'b'}, {id = 'c'}, {id = 'd'}
local function call(self) return self.id end
M.invoke({a,b,c,d},call) -- => "{'a','b','c','d'}"

local peoples = {
    {name = 'John', age = 23},{name = 'Peter', age = 17},
    {name = 'Steve', age = 15},{age = 33}}
M.pluck(peoples,'age') -- => "{23,17,15,33}"
M.pluck(peoples,'name') -- => "{'John', 'Peter', 'Steve'}"

M.max({1,2,3}) -- => 3
M.max({'a','b','c'}) -- => 'c'


M.max(peoples,function(people) return people.age end) -- => 33

M.min({1,2,3}) -- => 1
M.min({'a','b','c'}) -- => 'a'

local a = {'a','b','c','d'}
local b = {'b','a','d','c'}
print(M.same(a,b)) -- => true

b[#b+1] = 'e'
print(M.same(a,b)) -- => false

--M.sort({'b','a','d','c'}) -- => "{'a','b','c','d'}"

M.sort({'b','a','d','c'}, function(a,b) 
    return a:byte() > b:byte() 
end) -- => "{'d','c','b','a'}"

--local tbl = {}; tbl[3] = 5 ; tbl[2] = 6; tbl[5] = 8; tbl[4] = 10; tbl[1] = 12
--for k, v in M.sortedk(tbl) do print(k, v) end

--local function comp(a,b) return a > b end
--for k, v in M.sortedk(tbl, comp) do print(k, v) end

--local tbl = {}; tbl[3] = 5 ; tbl[2] = 6; tbl[5] = 8; tbl[4] = 10; tbl[1] = 12
--for k, v in M.sortedv(tbl) do print(k, v) end

--local function comp(a,b) return a > b end
--for k, v in M.sortedv(tbl, comp) do print(k, v) end

local r = M.sortBy({1,2,3,4,5}, math.sin)
print(table.concat(r,','))

local people = {
	{name = 'albert', age = 40},
	{name = 'louis', age = 55},
	{name = 'steve', age = 35},
	{name = 'henry', age = 19},
}
local r = M.sortBy(people, 'age')
--M.each(r, function(v) print(v.age, v.name)	end)

local r = M.sortBy(people, 'age', function(a,b) return a > b end)
--M.each(r, function(v) print(v.age, v.name)	end)

local r = M.sortBy({1,2,3,4,5})
print(table.concat(r,','))

M.groupBy({0,1,2,3,4,5,6},function(v)
    return v%2==0 and 'even' or 'odd'
end)

M.groupBy({0,'a',true, false,nil,b,0.5},type)

M.countBy({0,1,2,3,4,5,6},function(v) 
    return v%2==0 and 'even' or 'odd'
end)

M.size {1,2,3} -- => 3
M.size {one = 1, two = 2}

M.size {1,2,3} -- => 3
M.size {one = 1, two = 2}

M.contains({1,2,3,4},{1,2,3}) -- => true
M.contains({1,2,'d','b'},{1,2,3,5}) -- => true
M.contains({x = 1, y = 2, z = 3},{x = 1, y = 2})

M.sameKeys({1,2,3,4},{1,2,3}) -- => false
M.sameKeys({1,2,'d','b'},{1,2,3,5}) -- => true
M.sameKeys({x = 1, y = 2, z = 3},{x = 1, y = 2})