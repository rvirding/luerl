-- File    : moses-example.lua
-- Purpose : Brief demonstration of moses on Luerl.
-- See     : ./moses.erl


local M = require("moses")


-- Table functions


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


-- Array functions


local array = M.range(1,20)
local sample = M.sample(array, 3)
print(table.concat(sample,','))

local array = M.range(1,20)
local sample = M.sample(array)
print(sample)

local array = M.range(1,20)
local sample = M.sampleProb(array, 0.2)
print(table.concat(sample,','))

sample = M.sampleProb(array, 0.2, os.time())
print(table.concat(sample,','))

--local function comp(a,b) return a > b end
--M.nsorted(array,5, comp)

--local function comp(a,b) return a > b end
--M.nsorted(array,5, comp)

local list = M.shuffle {1,2,3,4,5,6} -- => "{3,2,6,4,1,5}"
M.each(list,print)

--M.pack(1,2,8,'d','a',0) -- => "{1,2,8,'d','a',0}"

local value = {3}
M.find({{4},{3},{2},{1}},value)

-- search value 4 starting from index 3
M.find({1,4,2,3,4,5},4,3) -- => 5

M.reverse({1,2,3,'d'}) -- => "{'d',3,2,1}"

local array = M.range(1,5)
M.fill(array, 0)

local array = M.range(1,5)
M.fill(array,0,3)

local array = M.range(1,5)
M.fill(array,0,2,4)

local array = M.range(1,5)
M.fill(array,0,5,10)

--M.zeros(4)

--M.ones(3)

--M.vector(10, 4)

M.selectWhile({2,4,5,8}, function(v)
    return v%2==0
end)

M.dropWhile({2,4,5,8}, function(v)
    return v%2==0
end)

M.sortedIndex({1,2,3},4)

M.indexOf({1,2,3},2)

M.lastIndexOf({1,2,2,3},2)

local array = {1,2,3,4,5,6}
local function multipleOf3(v) return v%3==0 end
M.findIndex(array, multipleOf3)

local array = {1,2,3,4,5,6}
local function multipleOf3(v) return v%3==0 end
M.findLastIndex(array, multipleOf3)

local array = {1}
M.addTop(array,1,2,3,4)

--local array = {'old_val'}
--M.prepend(array,1,2,3,4)

local array = {1}
M.push(array,1,2,3,4)

local array = {1,2,3}
local shift = M.shift(array)

local array = {1,2,3,4,5}
local a, b = M.shift(array, 2)

local array = {1,2,3}
local value = M.unshift(array)

M.pull({1,2,1,2,3,4,3},1,2,3)

local array = {1,2,3,4,5,6,7,8,9}
M.removeRange(array, 3,8)

local t = {1,5,2,4,3,3,4}
M.chunk(t, function(v) return v%2==0 end)

--local t = {1,5,2,4,3,3,4}
--M.chunk(t)

local array = {1,2,3,4,5,6,7,8,9}
M.slice(array, 3,6)

local array = {1,2,3,4,5,6,7,8,9}
M.first(array,3)

local array = {1,2,3,4,5,6,7,8,9}
M.initial(array,5) -- => "{1,2,3,4}"

local array = {1,2,3,4,5,6,7,8,9}
M.last(array,3) -- => "{7,8,9}"

local array = {1,2,3,4,5,6,7,8,9}
M.rest(array,6) -- => "{6,7,8,9}"

local array = {1,2,3,4,5,6}
M.nth(array,3) -- => "3"

M.compact {a,'aa',false,'bb',true} -- => "{'aa','bb',true}"

M.flatten({1,{2,3},{4,5,{6,7}}}) -- => "{1,2,3,4,5,6,7}"

M.flatten({1,{2},{{3}}},true) -- => "{1,{2},{{3}}}"

local array = {1,2,'a',4,5}
M.difference(array,{1,'a'}) -- => "{2,4,5}"

local A = {'a'}
local B = {'a',1,2,3}
local C = {2,10}
M.union(A,B,C) -- => "{'a',1,2,3,10}"

local A = {'a'}
local B = {'a',1,2,3}
local C = {2,10,1,'a'}
M.intersection(A,B,C) -- => "{'a'}"

--local A = {'a'}
--local B = {'a',1,3}
--local C = {3,10,2}

--M.disjoint(A,B) -- => false
--M.disjoint(A,C) -- => true
--M.disjoint(B,C) -- => false

local array = {1,2,3}
local array2 = {1,4,5}
M.symmetricDifference(array, array2) -- => "{2,3,4,5}"

M.unique({1,1,2,2,3,3,4,4,4,5}) -- => "{1,2,3,4,5}"

print(M.isunique({1,2,3,4,5})) -- => true
M.isunique({1,2,3,4,4}) -- => false

--M.duplicates({1,2,3,3,8,8,3,2,4}) -- => {2,3,8}

local names = {'Bob','Alice','James'}
local ages = {22, 23}
M.zip(names,ages) -- => "{{'Bob',22},{'Alice',23},{'James'}}"

--local names = {'Bob','Alice','James'}; local ages = {22, 23, 25}
--local function introduce(name, age) return 'I am '..name..' and I am '..age..' years old.' end
--local t = M.zipWith(introduce,names,ages)

M.append({1,2,3},{'a','b'}) -- => "{1,2,3,'a','b'}"

t1 = {1, 2, 3}
t2 = {'a', 'b', 'c'}
M.interleave(t1, t2) -- => "{1,'a',2,'b',3,'c'}"

M.interleave('a', {1,2,3}) -- => "{1,'a',2,'a',3}"

M.range(1,4) -- => "{1,2,3,4}"

M.range(3) -- => "{1,2,3}"

M.range(0,2,0.7) -- => "{0,0.7,1.4}"

M.range(-5) -- => "{-1,-2,-3,-4,-5}"

M.range(5,1) -- => "{5,4,3,2,1}"

M.rep(4,3)

-- M.powerset({1,2,3})

--local t = {1,2,3,4,5,6}
--for p in M.partition(t,2) do
--  print(table.concat(p, ','))
--end

-- => 1,2
-- => 3,4
-- => 5,6

--local t = {1,2,3,4,5,6}
--for p in M.partition(t,4) do
--  print(table.concat(p, ','))
--end

--local t = {1,2,3,4,5,6}
--for p in M.partition(t,4,0) do
--  print(table.concat(p, ','))
--end

--local t = {1,2,3,4,5,6,7}
--for p in M.overlapping(t,3) do
--	print(table.concat(p,','))
--end

--for p in M.overlapping(t,4) do
--	print(table.concat(p,','))
--end

--for p in M.overlapping(t,5) do
--	print(table.concat(p,','))
--end

--local t = {1,2,3,4,5,6,7}
--for p in M.overlapping(t,5,0) do
--	print(table.concat(p,','))
--end

--local t = {1,2,3,4,5}
--for p in M.aperture(t,4) do
--  print(table.concat(p,','))
--end

-- => 1,2,3,4
-- => 2,3,4,5

--for p in M.aperture(t,3) do
--  print(table.concat(p,','))
--end

--local t = M.range(5)
--for p in pairwise(t) do
--  print(table.concat(p,','))
--end

--t = {'a','b','c'}
--for p in M.permutation(t) do
--  print(table.concat(p))
--end

M.concat({'a',1,0,1,'b'})

--local t = M.xprod({1,2},{'a','b'})

--local t = M.xpairs(1, {1, 2, 3})

--local t = M.xpairsRight(1, {1, 2, 3})

--M.sum({1,2,3,4,5})

--M.product({1,2,3,4,5})

--M.mean({1,2,3,4,5})

--M.median({1,2,3,4,5})


-- Utility functions

