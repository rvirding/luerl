-- File    : moses-example.lua
-- Purpose : Brief demonstration of Moses on Luerl.
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

--local val = {-1, 8, 0, -6, 3, -1, 7, 1, -9}

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


M.noop()

M.identity(1)-- => 1
M.identity(false) -- => false
M.identity('hello!') -- => 'hello!'

--M.call(math.pow, 2, 3) -- => 8
--M.call(string.len, 'hello' ) -- => 5
--M.call(table.concat, {1,2,3,4,5}, ',', 2, 4) -- => {2,3,4}

local pi = M.constant(math.pi)
pi(1) -- => 3.1415926535898
pi(2) -- => 3.1415926535898
pi(math.pi) -- => 3.1415926535898

--local stats = M.applySpec({
--    min = function(...) return math.min(...) end,
--    max = function(...) return math.max(...) end,
--})

--stats(5,4,10,1,8) -- => {min = 1, max = 10}

--local function inc(x) return x + 1 end
--local function double(x) return 2 * x end
--local function square(x) return x * x end
--M.thread(2, inc, double, square) -- => 36
--M.thread(3, double, inc, square) -- => 49
--M.thread(4, square, double, inc) -- => 33
--M.thread(5, square, inc, double) -- => 52

--local function inc(x) return x + 1 end
--local function add(x, y) return x * y end
--local function pow(x, y) return x ^ y end
--M.thread(2, inc, {add, 3}, {pow, 2}) -- => 36
--M.thread(2, {add, 4}, inc, {pow, 2}) -- => 49

--local function inc(x) return x + 1 end
--local function add(x, y) return x * y end
--local function pow(x, y) return x ^ y end
--M.threadRight(2, inc, {add, 3}, {pow, 2}) -- => 64
--M.threadRight(2, {add, 4}, inc, {pow, 2}) -- => 128

--local f = M.dispatch(
--  function() return nil end,
--  function (v) return v+1 end, 
--  function (v) return 2*v end
--)
--f(5) -- => 6
--f(7) -- => 8

local function fibonacci(n)
    return n < 2 and n or fibonacci(n-1)+fibonacci(n-2)
end  
local mem_fibonacci = M.memoize(fibonacci)
fibonacci(20) -- => 6765 (but takes some time)
mem_fibonacci(20) -- => 6765 (takes less time)

--local function f(v)
--    if v < 100 then return v, v * 2 end
--end
--local t = M.unfold(f, 10) -- => {10,20,40,80}

local sq = M.once(function(a) return a*a end)
sq(1) -- => 1
sq(2) -- => 1
sq(3) -- => 1
sq(4) -- => 1
sq(5) -- => 1

local function greet(someone) return 'hello '..someone end
local greetOnly3people = M.before(greet, 3)
greetOnly3people('John') -- => 'hello John'
print(greetOnly3people('Moe')) -- => 'hello Moe'
greetOnly3people('James') -- => 'hello James'
greetOnly3people('Joseph') -- => 'hello James'
greetOnly3people('Allan') -- => 'hello James'

local f = M.after(M.identity,3)
f(1) -- => nil
f(2) -- => nil
f(3) -- => 3
f(4) -- => 4

local function f(x) return x^2 end
local function g(x) return x+1 end
local function h(x) return x/2 end
local compositae = M.compose(f,g,h)
compositae(10) -- => 36
compositae(20) -- => 121

local function f(x) return x^2 end
local function g(x) return x+1 end
local function h(x) return x/2 end
M.pipe(10,f,g,h) -- => 36
M.pipe(20,f,g,h) -- => 121

M.complement(function() return true end)()

local function f(x) return x^2 end
local function g(x) return x+1 end
local function h(x) return x/2 end
M.juxtapose(10, f, g, h) -- => 100, 11, 5

local greet = function(name) return "hi: " .. name end
local greet_backwards = M.wrap(greet, function(f,arg)
  return f(arg) ..'\nhi: ' .. arg:reverse()
end) 
print(greet_backwards('John'))

--local f = ('Lua programming'):gmatch('.')
--M.times(f, 3) -- => {'L','u','a'}

local sqrt2 = M.bind(math.sqrt,2)
print(sqrt2()) -- => 1.4142135623731

--local last2 = M.bind(M.last,2)
--last2({1,2,3,4,5,6}) -- => {5,6}

local function out(...) return table.concat {...} end
local out = M.bindn(out,'OutPut',':',' ')
out(1,2,3) -- => OutPut: 123
print(out('a','b','c','d')) -- => OutPut: abcd

print(M.uniqueId())

M.uniqueId('ID%s') -- => 'ID2'

local formatter = function(ID) return '$'..ID..'$' end
print(M.uniqueId(formatter)) -- => '$ID1$'

--local function po2(x) return x*2 end
--local function iter_po2 = M.iterator(po2, 1)
--iter_po2() -- => 2
--iter_po2() -- => 4
--iter_po2() -- => 8

--local function po2(x) return x*2 end
--local function iter_po2 = M.iterator(po2, 1, 3)
--iter_po2() -- => 2
--iter_po2() -- => 4
--iter_po2() -- => 8
--iter_po2() -- => nil

--local w = "hello"
--local char = string.gmatch(w,'.')
--local iter = M.skip(char, 3)
--for w in iter do print(w) end

--local text = 'letters'
--local chars = string.gmatch(text, '.')
--M.tabulate(chars) -- => {'l','e','t','t','e','r','s'}

--local text = 'lua'
--local chars = string.gmatch(text, '.')
--M.iterlen(chars) -- => 3
--chars() -- => nil

--M.castArray(true) -- => {true}
--M.castArray(2) -- => {2}

local function f(...) return table.concat({...}) end
local flipped = M.flip(f)
flipped('a','b','c') -- => 'cba'

--local f = M.nthArg(3)
--f('a','b','c') -- => 'c'

--local f = M.nthArg(-2)
--f('a','b','c') -- => 'b'

--local f = M.unary(function (...) return ... end)
--f('a') - ==> 'a'
--f('a','b','c') -- => 'a'

--local f = M.ary(function (...) return ... end, 2)
--f(1,2) - ==> 1,2
--f(1,2,3,4) -- => 1,2

--local f = M.unary(function (...) return ... end)
--f('a','b','c') -- => 'a'

--local f = M.noarg(function (x) return x or 'default' end)
--f(1) -- => 'default'
--f(function() end, 3) -- => 'default'

--local f = M.rearg(function (...) return ... end, {5,4,3,2,1})
--f('a','b','c','d','e') -- => 'e','d','c','b','a'

local minmax = M.over(math.min, math.max)
minmax(5,10,12,4,3) -- => {3,12}

local function alleven(...) 
	for i, v in ipairs({...}) do 
		if v%2~=0 then return false end 
	end 
	return true 
end

local function allpositive(...)
	for i, v in ipairs({...}) do 
		if v < 0 then return false end 
	end 
	return true 	
end

local allok = M.overEvery(alleven, allpositive)

allok(2,4,-1,8) -- => false
allok(10,3,2,6) -- => false
print(allok(8,4,6,10)) -- => true

local function alleven(...) 
	for i, v in ipairs({...}) do 
		if v%2~=0 then return false end 
	end 
	return true 
end

local function allpositive(...)
	for i, v in ipairs({...}) do 
		if v < 0 then return false end 
	end 
	return true 	
end

local anyok = M.overSome(alleven,allpositive)

anyok(2,4,-1,8) -- => false
anyok(10,3,2,6) -- => true
print(anyok(-1,-5,-3)) -- => false

local function f(x, y) return x, y end
local function triple(x) return x*3 end
local function square(x) return x^2 end
local new_f = M.overArgs(f, triple, square)

new_f(1,2) -- => 3, 4
new_f(10,10) -- => 30, 100

local function f(x, y, z) return x, y, z end
local function triple(x) return x*3 end
local function square(x) return x^2 end
local new_f = M.overArgs(f, triple, square)

new_f(1,2,3) -- => 3, 4, 3
new_f(10,10,10) -- => 30, 100, 10

--local function pow2(x) return x*x end
--local function pow3(x) return x*x*x end
--local function sum(a,b) return a+b end
--local poly = M.converge(sum, pow2, pow3)
--poly(5) -- => 150 (ie. 5*5 + 5*5*5)

local function diff(a, b) return a - b end
local diffFrom20 = M.partial(diff, 20) -- arg 'a' will be 20 by default
print(diffFrom20(5)) -- => 15

local function diff(a, b) return a - b end
local remove5 = M.partial(diff, '_', 5) -- arg 'a' will be given at call-time, but 'b' is set to 5
remove5(20) -- => 15

local function concat(...) return table.concat({...},',') end
local concat_right = M.partialRight(concat,'a','b','c')
concat_right('d') -- => d,a,b,c

concat_right = M.partialRight(concat,'a','b')
concat_right('c','d') -- => c,d,a,b

concat_right = M.partialRight(concat,'a')
concat_right('b','c','d') -- => b,c,d,a

local function concat(...) return table.concat({...},',') end
local concat_right = M.partialRight(concat,'a','_','c')
concat_right('d','b') -- => b,a,d,c

concat_right = M.partialRight(concat,'a','b','_')
concat_right('c','d') -- => d,a,b,c

concat_right = M.partialRight(concat,'_','a')
concat_right('b','c','d') -- => c,d,b,a

--local function sumOf3args(x,y,z) return x + y + z end
--local curried_sumOf3args = M.curry(sumOf3args, 3)
--sumOf3args(1)(2)(3)) -- => 6
--sumOf3args(0)(6)(9)) -- => 15

local function product(x,y) return x * y end
local curried_product = M.curry(product)
curried_product(5)(4) -- => 20
curried_product(3)(-5) -- => -15
curried_product(0)(1) -- => 0

local function wait_count(n) 
	local i = 0
	while i < n do i = i + 1 end
	return i
end

local time, i = M.time(wait_count, 1e6) -- => 0.002 1000000
print(time)


-- Object functions


M.keys({1,2,3}) -- => "{1,2,3}"
M.keys({x = 0, y = 1}) -- => "{'y','x'}"

M.values({1,2,3}) -- => "{1,2,3}"
M.values({x = 0, y = 1}) -- => "{1,0}"

--local entity = {
--    pos = {x = 1, y = 2},
--    engine = {
--        left = {status = 'active', damage = 5},
--        right = {status = 'off', damage = 10}
--    },
--    boost = false
--}

--M.path(entity,'pos','x') -- => 1
--M.path(entity,'pos','y') -- => 2
--M.path(entity,'engine','left','status') -- => 'active'
--M.path(entity,'engine','right','damage') -- => 10
--M.path(entity,'boost') -- => false

--local obj = {a = 1, b = 2, c = {d = 3, e = 4, f = {g = 5}}}
--M.spreadPath(obj, 'c', 'f')
-- => {a = 1, b = 2, d = 3, e = 4, g = 5, c = {f = {}}}

--local obj = {a = 1, b = 2, c = {d = 3, e = 4, f = {g = 5}}}
--M.spreadPath(obj, 'c', 'f')
-- => {a = 1, b = 2, d = 3, e = 4, g = 5, c = {d = 3, e = 4, f = {g = 5}}}

--local obj = {x = 1, y = 2, z = 3}
--M.each(M.kvpairs(obj), function(v,k)
--	print(k, table.concat(v,','))	
--end)

local list_pairs = {{'x',1},{'y',2},{'z',3}}
obj = M.toObj(list_pairs)

M.invert {'a','b','c'} -- => "{a=1, b=2, c=3}"
M.invert {x = 1, y = 2} -- => "{'x','y'}"

local who = M.property('name')
local people = {name = 'Henry'}
who(people) -- => 'Henry'

local people = {name = 'Henry'}
print(M.propertyOf(people)('name')) -- => 'Henry'

M.toBoolean(true) -- => true
M.toBoolean(false) -- => false
M.toBoolean(nil) -- => false
M.toBoolean({}) -- => true
M.toBoolean(1) -- => true

M.extend({},{a = 'b', c = 'd'}) -- => "{a = 'b', c = 'd'}"

local obj = {1,2,3}
local obj2 = M.clone(obj)
print(obj2 == obj) -- => false
print(M.isEqual(obj2, obj)) -- => true

--M.has(_,'has') -- => true
--M.has(coroutine,'resume') -- => true
--M.has(math,'random') -- => true

local object = {a = 1, b = 2, c = 3}
M.pick(object,'a','c') -- => "{a = 1, c = 3}"

local object = {a = 1, b = 2, c = 3}
M.omit(object,'a','c') -- => "{b = 2}"

local obj = {a = 0}
M.template(obj,{a = 1, b = 2, c = 3}) -- => "{a=0, c=3, b=2}"

M.isEqual(1,1) -- => true
M.isEqual(true,false) -- => false
M.isEqual(3.14,math.pi) -- => false
M.isEqual({3,4,5},{3,4,{5}}) -- => false

M.result('abc','len') -- => 3
M.result({'a','b','c'},table.concat) -- => 'abc'

M.isTable({}) -- => true
M.isTable(math) -- => true
M.isTable(string) -- => true

M.isCallable(print) -- => true
M.isCallable(function() end) -- => true
M.isCallable(setmetatable({},{__index = string}).upper) -- => true
M.isCallable(setmetatable({},{__call = function() return end})) -- => true

M.isArray({}) -- => true
M.isArray({1,2,3}) -- => true
M.isArray({'a','b','c'}) -- => true

M.isIterable({}) -- => true
M.isIterable(function() end) -- => false
M.isIterable(false) -- => false
M.isIterable(1) -- => false

--M.type('string') -- => 'string'
--M.type(table) -- => 'table'
--M.type(function() end) -- => 'function'
--M.type(io.open('f','w')) -- => 'file'

M.isEmpty('') -- => true
M.isEmpty({})  -- => true
M.isEmpty({'a','b','c'}) -- => false

M.isString('') -- => true
M.isString('Hello') -- => false
M.isString({}) -- => false

M.isFunction(print) -- => true
M.isFunction(function() end) -- => true
M.isFunction({}) -- => false

M.isNil(nil) -- => true
M.isNil() -- => true
M.isNil({}) -- => false

--M.isNumber(math.pi) -- => true
--M.isNumber(math.huge) -- => true
--M.isNumber(0/0) -- => true
--M.isNumber() -- => false

--M.isNaN(1) -- => false
--M.isNaN(0/0) -- => true

--M.isFinite(99e99) -- => true
--M.isFinite(math.pi) -- => true
--M.isFinite(math.huge) -- => false
--M.isFinite(1/0) -- => false
--M.isFinite(0/0) -- => false

M.isBoolean(true) -- => true
M.isBoolean(false) -- => true
M.isBoolean(1==1) -- => true
M.isBoolean(print) -- => false

M.isInteger(math.pi) -- => false
M.isInteger(1) -- => true
M.isInteger(-1) -- => true
