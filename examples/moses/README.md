__Moses__: A Lua utility-belt library for functional programming.

It complements built-in Lua functions, making easier common operations on tables, arrays, lists, collections, objects, and a lot more.

Tested on Luerl 5.3 this reference is copied from [here](https://github.com/Yonaba/Moses/blob/master/doc/tutorial.md), it show what works under Luerl using the library and document [what's not](#not-work)!

## <a name='TOC'>Installation</a>

Drop the file [moses.lua](http://github.com/Yonaba/Moses/blob/master/moses.lua) into your project and add it to your code with the *require* function:

```lua
local M = require("moses")
````

*Note:* [moses.lua](https://github.com/Yonaba/Moses/blob/master/moses.lua) is quite heavy (~92 kiB, 3115 LOC). You can alternatively use the [minified version](https://github.com/Yonaba/Moses/blob/master/moses_min.lua) (~35 kiB, 561 LOC).

*Moses* provides a large set of functions that can be classified into four categories:

* [__Table functions__](#table), which are mostly meant for tables, i.e Lua tables which contains both an array-part and a hash-part,
* [__Array functions__](#array), meant for array lists (or sequences),
* [__Utility functions__](#utility),
* [__Object functions__](#object).
<br><br>
## <a name='table'>Table functions</a>

### clear (t)

Clears a table. All its values becomes nil. Returns the passed-in table.

```lua
M.clear({1,2,'hello',true}) -- => {}
````

### each (t, f)
*Aliases: `forEach`*.

Iterates over each value-key pair in the passed-in table.

```lua
M.each({4,2,1},print)

-- => 4 1
-- => 2 2
-- => 1 3
````

The table can be map-like (both array part and hash part).

```lua
M.each({one = 1, two = 2, three = 3},print)

-- => 1 one
-- => 2 two
-- => 3 three
````

### at (t, ...)

Collects values at given keys and returns them in an array.

```lua
local t = {4,5,6}
M.at(t,1,3) -- => "{4,6}"

local t = {a = 4, bb = true, ccc = false}
M.at(t,'a', 'ccc') -- => "{4, false}"
````

### count (t [, val])

Counts the number of occurences of a given value in a table.

```lua
M.count({1,1,2,3,3,3,2,4,3,2},1) -- => 2
M.count({1,1,2,3,3,3,2,4,3,2},2) -- => 3
M.count({1,1,2,3,3,3,2,4,3,2},3) -- => 4
M.count({false, false, true},false) -- => 2
M.count({false, false, true},true) -- => 1
````

Returns the size of the list in case no value was provided.

```lua
M.count({1,1,2,3,3}) -- => 5
````

### countf (t, f)

Counts the number of values passing an iterator test.

```lua
M.countf({1,2,3,4,5,6}, function(v)
  return v%2==0
end) -- => 3

M.countf({print, pairs, os, assert, ipairs}, function(v)
  return type(v)=='function'
end) -- => 4
````

### cycle (t [, n = 1])
*Aliases: `loop`*.

Returns a function which iterates on each value-key pair in a given table (similarly to `M.each`), except that it restarts iterating again `n` times.
If `n` is not provided, it defaults to 1.

```lua
local t = {'a','b','c'}
for v in M.cycle(t, 2) do
  print(v)
end

-- => 'a'
-- => 'b'
-- => 'c'
-- => 'a'
-- => 'b'
-- => 'c'
````

Supports array-like tables and map-like tables.

```lua
local t = {x = 1, y = 2, z = 3}
for v in M.cycle(t) do
  print(v)
end

-- => 2
-- => 1
-- => 3
````

### map (t, f)
*Aliases: `collect`*.

Executes a function on each value in a given array.

```lua
M.map({1,2,3},function(v) 
  return v+10 
end) -- => "{11,12,13}"
````

```lua
M.map({a = 1, b = 2},function(v, k) 
  return k..v 
end) -- => "{a = 'a1', b = 'b2'}"
````

It also maps both keys and values.

```lua
M.map({a = 1, b = 2},function(v, k) 
  return k..k, v*2 
end) -- => "{aa = 2, bb = 4}"
````

### reduce (t, f [, state = next(t)])
*Aliases: `inject`, `foldl`*.

Can sum all values in a table. In case `state` is not provided, it defaults to the first value in the given table `t`.

```lua
local function add(a,b) return a+b end
M.reduce({1,2,3,4},add) -- => 10
````

Or concatenates all values.

```lua
local function concat(a,b) return a..b end	
M.reduce({'a','b','c','d'},concat) -- => abcd	 
````


### reduceRight (t, f [, state = next(t)])
*Aliases: `injectr`, `foldr`*.

Similar to `M.reduce`, but performs from right to left.

```lua
local initial_state = 256
local function div(a,b) return a/b end
M.reduceRight({1,2,4,16},div,initial_state) -- => 2
````

### mapReduce (t, f [, state = next(t)])
*Aliases: `mapr`*.

Reduces while saving intermediate states.

```lua
local function concat(a,b) return a..b end
M.mapReduce({'a','b','c'},concat) -- => "{'a', 'ab', 'abc'}"
````

### mapReduceRight (t, f [, state = next(t)])
*Aliases: `maprr`*.

Reduces from right to left, while saving intermediate states.

```lua
local function concat(a,b) return a..b end
M.mapReduceRight({'a','b','c'},concat) -- => "{'c', 'cb', 'cba'}"
````

### include (t, value)
*Aliases: `any`, `some`, `contains`*.

Looks for a value in a table.

```lua
M.include({6,8,10,16,29},16) -- => true
M.include({6,8,10,16,29},1) -- => false

local complex_table = {18,{2,{3}}}
local collection = {6,{18,{2,6}},10,{18,{2,{3}}},29}
M.include(collection, complex_table) -- => true
````

Handles iterator functions.

```lua
local function isUpper(v) return v:upper()== v end
M.include({'a','B','c'},isUpper) -- => true
````

### detect (t, value)

Returns the index of a value in a table.

```lua
M.detect({6,8,10,16},8) -- => 2
M.detect({nil,true,0,true,true},false) -- => nil

local complex_table = {18,{2,6}}
local collection = {6,{18,{2,6}},10,{18,{2,{3}}},29}
M.detect(collection, complex_table) -- => 2
````

Handles iterator functions.

```lua
local function isUpper(v)
  return v:upper()==v
end
M.detect({'a','B','c'},isUpper) -- => 2
````

### where (t, props)

Looks through a table and returns all the values that matches all of the key-value pairs listed in `props`. 

```lua
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
````

### findWhere (t, props)

Looks through a table and returns the first value found that matches all of the key-value pairs listed in `props`. 

```lua
local a = {a = 1, b = 2, c = 3}
local b = {a = 2, b = 3, d = 4}
local c = {a = 3, b = 4, e = 5}
M.findWhere({a, b, c}, {a = 3, b = 4}) == c -- => true
````

### select (t, f)
*Aliases: `filter`*.

Collects values passing a validation test.

```lua
local function isEven(v) return v%2==0 end
local function isOdd(v) return v%2~=0 end

M.select({1,2,3,4,5,6,7}, isEven) -- => "{2,4,6}"
M.select({1,2,3,4,5,6,7}, isOdd) -- => "{1,3,5,7}"
````

### reject (t, f)
*Aliases: `reject`*.

Removes all values failing (returning false or nil) a validation test:

```lua
local function isEven(v) return v%2==0 end
local function isOdd(v) return v%2~=0 end

M.reject({1,2,3,4,5,6,7}, isEven) -- => "{1,3,5,7}"
M.reject({1,2,3,4,5,6,7}, isOdd) -- => "{2,4,6}"
````

### all (t, f)
*Aliases: `every`*.

Checks whether or not all elements pass a validation test.

```lua
local function isEven(v) return v%2==0 end
M.all({2,4,6}, isEven) -- => true
````

### invoke (t, method)

Invokes a given function on each value in a table.

```lua
M.invoke({'a','bea','cdhza'},string.len) -- => "{1,3,5}"
````

Can reference the method of the same name in each value.

```lua
local a, b, c, d = {id = 'a'}, {id = 'b'}, {id = 'c'}, {id = 'd'}
local function call(self) return self.id end
M.invoke({a,b,c,d},call) -- => "{'a','b','c','d'}"
````

### pluck (t, property)

Fetches all values indexed with specific key in a table of objects.

```lua
local peoples = {
  {name = 'John', age = 23},{name = 'Peter', age = 17},
  {name = 'Steve', age = 15},{age = 33}}

M.pluck(peoples,'age') -- => "{23,17,15,33}"
M.pluck(peoples,'name') -- => "{'John', 'Peter', 'Steve'}"
````

### max (t [, transform])

Returns the maximum value in a collection.

```lua
M.max {1,2,3} -- => 3
M.max {'a','b','c'} -- => 'c'
````

Can take an iterator function to extract a specific property.

```lua
local peoples = {
  {name = 'John', age = 23},{name = 'Peter', age = 17},
  {name = 'Steve', age = 15},{age = 33}}
M.max(peoples,function(people) return people.age end) -- => 33
````

### min (t [, transform])

Returns the minimum value in a collection.

```lua
M.min {1,2,3} -- => 1
M.min {'a','b','c'} -- => 'a'
````

Can take an iterator function to extract a specific property.

```lua
local peoples = {
  {name = 'John', age = 23},{name = 'Peter', age = 17},
  {name = 'Steve', age = 15},{age = 33}}
M.min(peoples,function(people) return people.age end) -- => 15
````

### same (a, b)

Tests whether or not all values in each of the passed-in tables exists in both tables.

```lua
local a = {'a','b','c','d'}      
local b = {'b','a','d','c'}
M.same(a,b) -- => true

b[#b+1] = 'e'
M.same(a,b) -- => false
````

### sort (t [, comp = math.min])

Sorts a collection.

```lua
M.sort({'b','a','d','c'}) -- => "{'a','b','c','d'}"
````

Handles custom comparison functions.

```lua
M.sort({'b','a','d','c'}, function(a,b) 
  return a:byte() > b:byte() 
end) -- => "{'d','c','b','a'}"
````

### sortBy (t [, transform [, comp = math.min]])

Sorts items in a collection based on the result of running a transform function through every item in the collection.

```lua
local r = M.sortBy({1,2,3,4,5}, math.sin)
print(table.concat(r,','))

-- => {5,4,3,1,2}
````

The transform function can also be a string name property.

```lua
local people = {
	{name = 'albert', age = 40},
	{name = 'louis', age = 55},
	{name = 'steve', age = 35},
	{name = 'henry', age = 19},
}
local r = M.sortBy(people, 'age')
M.each(r, function(v) print(v.age, v.name)	end)

-- => 19	henry
-- => 35	steve
-- => 40	albert
-- => 55	louis
````

As seen above, the defaut comparison function is the '<' operator. For example, let us supply a different one to sort the list of people by decreasing age order :

```lua
local people = {
	{name = 'albert', age = 40},
	{name = 'louis', age = 55},
	{name = 'steve', age = 35},
	{name = 'henry', age = 19},
}
local r = M.sortBy(people, 'age', function(a,b) return a > b end)
M.each(r, function(v) print(v.age, v.name)	end)

-- => 55	louis
-- => 40	albert
-- => 35	steve
-- => 19	henry
````

The `transform` function defaults to `M.indentity` and in that case, `M.sortBy` behaves like `M.sort`.

```lua
local r = M.sortBy({1,2,3,4,5})
print(table.concat(r,','))

-- => {1,2,3,4,5}
````

### groupBy (t, iter)

Groups values in a collection depending on their return value when passed to a predicate test.

```lua
M.groupBy({0,1,2,3,4,5,6},function(v) 
  return v%2==0 and 'even' or 'odd'
end)
-- => "{odd = {1,3,5}, even = {0,2,4,6}}"

M.groupBy({0,'a',true, false,nil,b,0.5},type) 
-- => "{number = {0,0.5}, string = {'a'}, boolean = {true, false}}"		
````

### countBy (t, iter)

Splits a table in subsets and provide the count for each subset.

```lua
M.countBy({0,1,2,3,4,5,6},function(v) 
  return v%2==0 and 'even' or 'odd'
end) -- => "{odd = 3, even = 4}"
````

### size (...)

When given a table, provides the count for the very number of values in that table.

```lua
M.size {1,2,3} -- => 3
M.size {one = 1, two = 2} -- => 2
````

When given a vararg list of arguments, returns the count of these arguments.

```lua
M.size(1,2,3) -- => 3
M.size('a','b',{}, function() end) -- => 4
````

### containsKeys (t, other)

Checks whether a table has all the keys existing in another table.

```lua
M.contains({1,2,3,4},{1,2,3}) -- => true
M.contains({1,2,'d','b'},{1,2,3,5}) -- => true
M.contains({x = 1, y = 2, z = 3},{x = 1, y = 2}) -- => true
````

### sameKeys (tA, tB)

Checks whether both tables features the same keys:

```lua
M.sameKeys({1,2,3,4},{1,2,3}) -- => false
M.sameKeys({1,2,'d','b'},{1,2,3,5}) -- => true
M.sameKeys({x = 1, y = 2, z = 3},{x = 1, y = 2}) -- => false
````

**[[⬆]](#TOC)**

## <a name='array'>Array functions</a>

### sample (array [, n = 1 [, seed]])

Samples `n` values from array.

```lua
local array = M.range(1,20)
local sample = M.sample(array, 3)
print(table.concat(sample,','))

-- => {12,11,15}
````

`n` defaults to 1. In that case, a single value will be returned.

```lua
local array = M.range(1,20)
local sample = M.sample(array)
print(sample)

-- => 12
````

An optional 3rd argument `seed` can be passed for deterministic random sampling.

### sampleProb (array, prob [, seed])

Returns an array of values randomly selected from a given array.
In case `seed` is provided, it is used for deterministic sampling.

```lua
local array = M.range(1,20)
local sample = M.sampleProb(array, 0.2)
print(table.concat(sample,','))

-- => 5,11,12,15

sample = M.sampleProb(array, 0.2, os.time())
print(table.concat(sample,','))

-- => 1,6,10,12,15,20 (or similar)
````

### shuffle (array [, seed])

Shuffles a given array.

```lua
local list = M.shuffle {1,2,3,4,5,6} -- => "{3,2,6,4,1,5}"
M.each(list,print)
````

### pack (...)

Converts a vararg list of arguments to an array.

```lua
M.pack(1,2,8,'d','a',0) -- => "{1,2,8,'d','a',0}"
````

### find (array, value [, from = 1])

Looks for a value in a given array and returns the position of the first occurence.

```lua
local value = {3}
M.find({{4},{3},{2},{1}},value) -- => 2
````

It can also start the search at a specific position in the array:

```lua
-- search value 4 starting from index 3
M.find({1,4,2,3,4,5},4,3) -- => 5
````

### reverse (array)

Reverses an array.

```lua
M.reverse({1,2,3,'d'}) -- => "{'d',3,2,1}"
````

### fill (array, value [, i = 1 [, j = #array]])

Replaces all elements in a given array with a given value.

```lua
local array = M.range(1,5)
M.fill(array, 0) -- => {0,0,0,0,0}
````

It can start replacing value at a specific index.

```lua
local array = M.range(1,5)
M.fill(array,0,3) -- => {1,2,0,0,0}
````

It can replace only values within a specific range.

```lua
local array = M.range(1,5)
M.fill(array,0,2,4) -- => {1,0,0,0,5}
````

In case the upper bound index i greather than the array size, it will enlarge the array.

```lua
local array = M.range(1,5)
M.fill(array,0,5,10) -- => {1,2,3,4,0,0,0,0,0,0}
````

### selectWhile (array, f [, ...])
*Aliases: `takeWhile`*.

Collects values as long as they pass a given test. Stops on the first non-passing test.

```lua
M.selectWhile({2,4,5,8}, function(v)
  return v%2==0
end) -- => "{2,4}"
````

### dropWhile (array, f [, ...])
*Aliases: `rejectWhile`*.

Removes values as long as they pass a given test. Stops on the first non-passing test.

```lua
M.dropWhile({2,4,5,8}, function(v)
  return v%2==0
end) -- => "{5,8}"
````

### sortedIndex (array, value [, comp = math.min [, sort = nil]])

Returns the index at which a value should be inserted to preserve order.

```lua
M.sortedIndex({1,2,3},4) -- => 4
````

Can take a custom comparison functions.

```lua
local comp = function(a,b) return a<b end
M.sortedIndex({-5,0,4,4},3,comp) -- => 3
````

### indexOf (array, value)

Returns the index of a value in an array.

```lua
M.indexOf({1,2,3},2) -- => 2
````

### lastIndexOf (array, value)

Returns the index of the last occurence of a given value in an array.

```lua
M.lastIndexOf({1,2,2,3},2) -- => 3
````

### findIndex (array, pred)

Returns the first index at which a predicate passes a truth test.

```lua
local array = {1,2,3,4,5,6}
local function multipleOf3(v) return v%3==0 end
M.findIndex(array, multipleOf3) -- => 3
````

### findLastIndex (array, pred)

Returns the last index at which a predicate passes a truthy test.

```lua
local array = {1,2,3,4,5,6}
local function multipleOf3(v) return v%3==0 end
M.findLastIndex(array, multipleOf3) -- => 6
````

### addTop (array, ...)

Adds given values at the top of an array. The latter values bubbles at the top.

```lua
local array = {1}
M.addTop(array,1,2,3,4) -- => "{4,3,2,1,1}"
````

### push (array, ...)

Adds given values at the end of an array.

```lua
local array = {1}
M.push(array,1,2,3,4) -- => "{1,1,2,3,4}"
````

### shift (array [, n = 1])
*Aliases: `pop`*.

Removes and returns the first value in an array.

```lua
local array = {1,2,3}
local shift = M.shift(array) -- => "shift = 1", "array = {2,3}"
````
If `n` is supplied, returns `n` values.

```lua
local array = {1,2,3,4,5}
local a, b = M.shift(array, 2) -- => "a = 1, b = 2", "array = {3,4,5}"
````

### unshift (array [, n = 1])

Removes and returns the last value in an array.

```lua
local array = {1,2,3}
local value = M.unshift(array) -- => "value = 3", "array = {1,2}"
````

### pull (array, ...)
*Aliases: `remove`*.

Removes all provided values from a given array.

```lua
M.pull({1,2,1,2,3,4,3},1,2,3) -- => "{4}"
````

### removeRange (array [, start = 1 [, finish = #array]])
*Aliases: `rmRange`, `M.chop`*.

Trims out all values index within a range.

```lua
local array = {1,2,3,4,5,6,7,8,9}
M.removeRange(array, 3,8) -- => "{1,2,9}"
````

### slice (array [, start = 1 [, finish = #array]])
*Aliases: `sub`*.

Slices and returns a part of an array.

```lua
local array = {1,2,3,4,5,6,7,8,9}
M.slice(array, 3,6) -- => "{3,4,5,6}"
````

### first (array [, n = 1])
*Aliases: `head`, `M.take`*.

Returns the first N elements in an array.

```lua
local array = {1,2,3,4,5,6,7,8,9}
M.first(array,3) -- => "{1,2,3}"
````

### initial (array [, n = #array])

Excludes the last N elements in an array.

```lua
local array = {1,2,3,4,5,6,7,8,9}
M.initial(array,5) -- => "{1,2,3,4}"
````

### last (array [, n = #array])

Returns the last N elements in an array.

```lua
local array = {1,2,3,4,5,6,7,8,9}
M.last(array,3) -- => "{7,8,9}"
````

### rest (array [, index = 1])
*Aliases: `tail`*.

Returns all values after *index*, including the given *index* itself.

```lua
local array = {1,2,3,4,5,6,7,8,9}
M.rest(array,6) -- => "{6,7,8,9}"
````

### nth (array, index)

Returns the value at *index*.

```lua
local array = {1,2,3,4,5,6}
M.nth(array,3) -- => "3"
````

### compact (array)

Trims out all falsy values.

```lua
M.compact {a,'aa',false,'bb',true} -- => "{'aa','bb',true}"
````

### flatten (array [, shallow = false])

Flattens a nested array.

```lua
M.flatten({1,{2,3},{4,5,{6,7}}}) -- => "{1,2,3,4,5,6,7}"
````

When given arg `shallow`, flatten only at the first level.

```lua
M.flatten({1,{2},{{3}}},true) -- => "{1,{2},{{3}}}"
````

### difference (array, array2)
*Aliases: `without`, `diff`*.

Returns values in the given array not present in a second array.

```lua
local array = {1,2,'a',4,5}
M.difference(array,{1,'a'}) -- => "{2,4,5}"
````

### union (...)

Produces a duplicate-free union of all passed-in arrays.

```lua
local A = {'a'}
local B = {'a',1,2,3}
local C = {2,10}
M.union(A,B,C) -- => "{'a',1,2,3,10}"
````

### intersection (...)

Returns the intersection (common-part) of all passed-in arrays:

```lua
local A = {'a'}
local B = {'a',1,2,3}
local C = {2,10,1,'a'}
M.intersection(A,B,C) -- => "{'a'}"
````

### symmetricDifference (array, array2)
*Aliases: `symdiff`,`xor`*.

Returns values in the first array not present in the second and also values in the second array not present in the first one.

```lua
local array = {1,2,3}
local array2 = {1,4,5}
M.symmetricDifference(array, array2) -- => "{2,3,4,5}"
````

### unique (array)
*Aliases: `uniq`*.

Makes an array duplicate-free.

```lua
M.unique {1,1,2,2,3,3,4,4,4,5} -- => "{1,2,3,4,5}"
````

### isunique (array)
*Aliases: `isuniq`*.

Checks if a given array contains no duplicate value.

```lua
M.isunique({1,2,3,4,5}) -- => true
M.isunique({1,2,3,4,4}) -- => false
````

### zip (...)
*Aliases: `transpose`*.

Zips values from different arrays, on the basis on their common keys.

```lua
local names = {'Bob','Alice','James'}
local ages = {22, 23}
M.zip(names,ages) -- => "{{'Bob',22},{'Alice',23},{'James'}}"
````

### append (array, other)

Appends two arrays.

```lua
M.append({1,2,3},{'a','b'}) -- => "{1,2,3,'a','b'}"
````

### interleave (...)

Interleaves values from passed-in arrays.

```lua
t1 = {1, 2, 3}
t2 = {'a', 'b', 'c'}
M.interleave(t1, t2) -- => "{1,'a',2,'b',3,'c'}"
````

### interpose (array, value)
*Aliases: `intersperce`*.

Interposes a value between consecutive values in an arrays.

```lua
M.interleave('a', {1,2,3}) -- => "{1,'a',2,'a',3}"
````

### range ([from [, to [, step]]])

Generates an arithmetic sequence.

```lua
M.range(1,4) -- => "{1,2,3,4}"
````

In case a single value is provided, it generates a sequence from 1 to that value.

````
M.range(3) -- => "{1,2,3}"
````

The incremental step can also be provided as third argument.

```lua
M.range(0,2,0.7) -- => "{0,0.7,1.4}"
````

It also handles negative progressions.

```lua
M.range(-5) -- => "{-1,-2,-3,-4,-5}"
M.range(5,1) -- => "{5,4,3,2,1}"
````

### rep (value, n)

Generates a list of n repetitions of a value.

```lua
M.rep(4,3) -- => "{4,4,4}"
````

### concat (array [, sep = '' [, i = 1 [, j = #array]]])
*Aliases: `join`*.

Concatenates a given array values:

```lua
M.concat({'a',1,0,1,'b'}) -- => 'a101b'
````


**[[⬆]](#TOC)**

## <a name='utility'>Utility functions</a>

### noop ()

The no-operation function. Takes nothing, returns nothing. It is being used internally.

```lua
M.noop() -- => nil
````

### identity (value)

Returns the passed-in value. <br/>
This function is internally used as a default transformation function.

```lua
M.identity(1)-- => 1
M.identity(false) -- => false
M.identity('hello!') -- => 'hello!'
````

### constant (value)

Creates a constant function. This function will continuously yield the same output.

```lua
local pi = M.constant(math.pi)
pi(1) -- => 3.1415926535898
pi(2) -- => 3.1415926535898
pi(math.pi) -- => 3.1415926535898
````

### memoize (f)
*Aliases: `cache`*.

Memoizes a slow-running function. It caches the result for a specific input, so that the next time the function is called with the same input, it will lookup the result in its cache, instead of running again the function body.

```lua
local function fibonacci(n)
  return n < 2 and n or fibonacci(n-1)+fibonacci(n-2)
end  
local mem_fibonacci = M.memoize(fibonacci)
fibonacci(20) -- => 6765 (but takes some time)
mem_fibonacci(20) -- => 6765 (takes less time)
````


### once (f)

Produces a function that runs only once. Successive calls to this function will still yield the same input.

```lua
local sq = M.once(function(a) return a*a end)
sq(1) -- => 1
sq(2) -- => 1
sq(3) -- => 1
sq(4) -- => 1
sq(5) -- => 1
````

### before (f, count)

Returns a version of `f` that will run no more than `count` times. Next calls will keep yielding the results of the (n-th)-1 call.

```lua
local function greet(someone) return 'hello '..someone end
local greetOnly3people = M.before(greet, 3)
greetOnly3people('John') -- => 'hello John'
greetOnly3people('Moe') -- => 'hello Moe'
greetOnly3people('James') -- => 'hello James'
greetOnly3people('Joseph') -- => 'hello James'
greetOnly3people('Allan') -- => 'hello James'
````

### after (f, count)

Produces a function that will respond only after a given number of calls.

```lua
local f = M.after(M.identity,3)
f(1) -- => nil
f(2) -- => nil
f(3) -- => 3
f(4) -- => 4
````

### compose (...)

Composes functions. Each function consumes the return value of the one that follows.

```lua
local function f(x) return x^2 end
local function g(x) return x+1 end
local function h(x) return x/2 end
local compositae = M.compose(f,g,h)
compositae(10) -- => 36
compositae(20) -- => 121
````

### pipe (value, ...)

Pipes a value through a series of functions.

```lua
local function f(x) return x^2 end
local function g(x) return x+1 end
local function h(x) return x/2 end
M.pipe(10,f,g,h) -- => 36
M.pipe(20,f,g,h) -- => 121
````

### complement (f)

Returns a function which returns the logical complement of a given function.

```lua
M.complement(function() return true end)() -- => false
````

### juxtapose (value, ...)
*Aliases: `juxt`*.

Calls a sequence of functions with the same input.

```lua
local function f(x) return x^2 end
local function g(x) return x+1 end
local function h(x) return x/2 end
M.juxtapose(10, f, g, h) -- => 100, 11, 5
````

### wrap (f, wrapper)

Wraps a function inside a wrapper. Allows the wrapper to execute code before and after function run.

```lua
local greet = function(name) return "hi: " .. name end
local greet_backwards = M.wrap(greet, function(f,arg)
  return f(arg) ..'\nhi: ' .. arg:reverse()
end) 
greet_backwards('John')

-- => hi: John
-- => hi: nhoJ
````

### bind (f, v)

Binds a value to be the first argument to a function.

```lua
local sqrt2 = M.bind(math.sqrt,2)
sqrt2() -- => 1.4142135623731
````

### bindn (f, ...)

Binds a variable number of values to be the first arguments to a function.

```lua
local function out(...) return table.concat {...} end
local out = M.bindn(out,'OutPut',':',' ')
out(1,2,3) -- => OutPut: 123
out('a','b','c','d') -- => OutPut: abcd
````

### uniqueId ([template])
*Aliases: `uid`*.

Returns an unique integer ID.

```lua
M.uniqueId() -- => 1
````

Can handle string templates for formatted output.

```lua
M.uniqueId('ID%s') -- => 'ID2'
````

Or a function, for the same purpose.

```lua
local formatter = function(ID) return '$'..ID..'$' end
M.uniqueId(formatter) -- => '$ID1$'
````

### flip (f)

Creates a function of `f` with arguments flipped in reverse order.

```lua
local function f(...) return table.concat({...}) end
local flipped = M.flip(f)
flipped('a','b','c') -- => 'cba'
````

### over (...)

Creates a function that invokes a set of transforms with the arguments it receives.<br/>
One can use use for example to get the tuple of min and max values from a set of values

```lua
local minmax = M.over(math.min, math.max)
minmax(5,10,12,4,3) -- => {3,12}
````

### overEvery (...)

Creates a validation function. The returned function checks if all of the given predicates return truthy when invoked with the arguments it receives.

```lua
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
allok(8,4,6,10) -- => true
````

### overSome (...)

Creates a validation function. The returned function checks if any of the given predicates return truthy when invoked with the arguments it receives.

```lua
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
anyok(-1,-5,-3) -- => false
````

### overArgs (f, ...)

Creates a function that invokes `f` with its arguments transformed

```lua
local function f(x, y) return x, y end
local function triple(x) retun x*3 end
local function square(x) retun x^2 end
local new_f = M.overArgs(f, triple, square)

new_f(1,2) -- => 3, 4
new_f(10,10) -- => 30, 100
````

In case the number of arguments is greater than the number of transforms, the remaining args will be left as-is.

```lua
local function f(x, y, z) return x, y, z end
local function triple(x) retun x*3 end
local function square(x) retun x^2 end
local new_f = M.overArgs(f, triple, square)

new_f(1,2,3) -- => 3, 4, 3
new_f(10,10,10) -- => 30, 100, 10
````

### partial (f, ...)

Partially apply a function by filling in any number of its arguments. 

```lua
local function diff(a, b) return a - b end
local diffFrom20 = M.partial(diff, 20) -- arg 'a' will be 20 by default
diffFrom20(5) -- => 15
````

The string `'_'` can be used as a placeholder in the list of arguments to specify an argument that should not be pre-filled, but is rather left open to be supplied at call-time.

```lua
local function diff(a, b) return a - b end
local remove5 = M.partial(diff, '_', 5) -- arg 'a' will be given at call-time, but 'b' is set to 5
remove5(20) -- => 15
````

### partialRight (f, ...)

Like `M.partial`, it partially applies a function by filling in any number of its arguments, but from the right.

```lua
local function concat(...) return table.concat({...},',') end
local concat_right = M.partialRight(concat,'a','b','c')
concat_right('d') -- => d,a,b,c

concat_right = M.partialRight(concat,'a','b')
concat_right('c','d') -- => c,d,a,b

concat_right = M.partialRight(concat,'a')
concat_right('b','c','d') -- => b,c,d,a
```

The string `'_'`, as always, can be used as a placeholder in the list of arguments to specify an argument that should not be pre-filled, but is rather left open to be supplied at call-time.
In that case, the first args supplied at runtime will be used to fill the initial list of args while the remaining will be prepended.

```lua
local function concat(...) return table.concat({...},',') end
local concat_right = M.partialRight(concat,'a','_','c')
concat_right('d','b') -- => b,a,d,c

concat_right = M.partialRight(concat,'a','b','_')
concat_right('c','d') -- => d,a,b,c

concat_right = M.partialRight(concat,'_','a')
concat_right('b','c','d') -- => c,d,b,a
````


### time (f [, ...])

Returns the execution time of `f (...)` in seconds and its results.

```lua
local function wait_count(n) 
	local i = 0
	while i < n do i = i + 1 end
	return i
end

local time, i = M.time(wait_count, 1e6) -- => 0.002 1000000
local time, i = M.time(wait_count, 1e7) -- => 0.018 10000000
````

**[[⬆]](#TOC)**

## <a name='object'>Object functions</a>

### keys (obj)

Collects the names of an object attributes.

```lua
M.keys({1,2,3}) -- => "{1,2,3}"
M.keys({x = 0, y = 1}) -- => "{'y','x'}"
````

### values (obj)

Collects the values of an object attributes.

```lua
M.values({1,2,3}) -- => "{1,2,3}"
M.values({x = 0, y = 1}) -- => "{1,0}"
````

### toObj (kvpairs)

Converts an array list of `kvpairs` to an object where keys are taken from the 1rst column in the `kvpairs` sequence, associated with values in the 2nd column.

```lua
local list_pairs = {{'x',1},{'y',2},{'z',3}}
obj = M.toObj(list_pairs)

-- => {x = 1, y = 2, z = 3}
````

### invert (obj)
*Aliases: `mirror`*.

Switches <tt>key-value</tt> pairs:

```lua
M.invert {'a','b','c'} -- => "{a=1, b=2, c=3}"
M.invert {x = 1, y = 2} -- => "{'x','y'}"
````

### property (key)

Returns a function that will return the key property of any passed-in object.

```lua
local who = M.property('name')
local people = {name = 'Henry'}
who(people) -- => 'Henry'
````

### propertyOf (obj)

Returns a function that will return the key property of any passed-in object.

```lua
local people = {name = 'Henry'}
print(M.propertyOf(people)('name')) -- => 'Henry'
````

### toBoolean (value)

Converts a given value to a boolean.

```lua
M.toBoolean(true) -- => true
M.toBoolean(false) -- => false
M.toBoolean(nil) -- => false
M.toBoolean({}) -- => true
M.toBoolean(1) -- => true
````

### extend (destObj, ...)

Extends a destination object with the properties of some source objects.

```lua
M.extend({},{a = 'b', c = 'd'}) -- => "{a = 'b', c = 'd'}"
````

### functions (obj [, recurseMt])
*Aliases: `methods`*.

Returns all functions names within an object.

```lua
M.functions(coroutine) 
-- => "{'yield','wrap','status','resume','running','create'}"
````

When given `recurseMt`, will also include `obj` metatable's functions.

````lua
local mt = {print = print}
local t = {assert = assert}
setmetatable(t, {__index = mt})
M.functions(t, true) -- => "{'assert','print'}"
````

### clone (obj [, shallow])

Clones a given object.

```lua
local obj = {1,2,3}
local obj2 = M.clone(obj)
print(obj2 == obj) -- => false
print(M.isEqual(obj2, obj)) -- => true
````

### tap (obj, f)

Invokes a given interceptor function on some object, and then returns the object itself. Useful to tap into method chaining to hook intermediate results.
The passed-in interceptor should be prototyped as `f(obj,...)`.

```lua
local v = M.chain({1,2,3,4,5,6,7,8,9,10})
  :filter(function(v) return v%2~=0 end) -- retain odd values
  :tap(function(v) print('Max is', M.max(v) end) -- Tap max value 
  :map(function(v) return v^2 end)
  :value() -- =>	 Max is 89
````

### pick (obj, ...)
*Aliases: `choose`*.

Collects whilelisted properties of a given object.

```lua
local object = {a = 1, b = 2, c = 3}
M.pick(object,'a','c') -- => "{a = 1, c = 3}"
````

### omit (obj, ...)
*Aliases: `drop`*.

Omits blacklisted properties of a given object.

```lua
local object = {a = 1, b = 2, c = 3}
M.omit(object,'a','c') -- => "{b = 2}"
````

### template (obj [, template])
*Aliases: `defaults`*.

Applies a template on an object, preserving existing properties.

```lua
local obj = {a = 0}
M.template(obj,{a = 1, b = 2, c = 3}) -- => "{a=0, c=3, b=2}"
````

### isEqual (objA, objB [, useMt])
*Aliases: `compare`, `M.matches`*.

Compares objects:

```lua
M.isEqual(1,1) -- => true
M.isEqual(true,false) -- => false
M.isEqual(3.14,math.pi) -- => false
M.isEqual({3,4,5},{3,4,{5}}) -- => false
````

### result (obj, method)

Calls an object method, passing it as a first argument the object itself.

```lua
M.result('abc','len') -- => 3
M.result({'a','b','c'},table.concat) -- => 'abc'
````

### isTable (t)

Is the given argument an object (i.e a table) ?

```lua
M.isTable({}) -- => true
M.isTable(math) -- => true
M.isTable(string) -- => true
````

### isCallable (obj)

Is the given argument callable ?

```lua
M.isCallable(print) -- => true
M.isCallable(function() end) -- => true
M.isCallable(setmetatable({},{__index = string}).upper) -- => true
M.isCallable(setmetatable({},{__call = function() return end})) -- => true
````

### isArray (obj)

Is the given argument an array (i.e. a sequence) ?

```lua
M.isArray({}) -- => true
M.isArray({1,2,3}) -- => true
M.isArray({'a','b','c'}) -- => true
````

### isIterable (obj)

Checks if the given object is iterable with `pairs`.

```lua
M.isIterable({}) -- => true
M.isIterable(function() end) -- => false
M.isIterable(false) -- => false
M.isIterable(1) -- => false
````

### isEmpty ([obj])

Is the given argument empty ?

```lua
M.isEmpty('') -- => true
M.isEmpty({})  -- => true
M.isEmpty({'a','b','c'}) -- => false
````

### isString (obj)

Is the given argument a string ?

```lua
M.isString('') -- => true
M.isString('Hello') -- => false
M.isString({}) -- => false
````

### isFunction (obj)

Is the given argument a function ?

```lua
M.isFunction(print) -- => true
M.isFunction(function() end) -- => true
M.isFunction({}) -- => false
````

### isNil (obj)

Is the given argument nil ?

```lua
M.isNil(nil) -- => true
M.isNil() -- => true
M.isNil({}) -- => false
````

### isBoolean (obj)

Is the given argument a boolean ?

```lua
M.isBoolean(true) -- => true
M.isBoolean(false) -- => true
M.isBoolean(1==1) -- => true
M.isBoolean(print) -- => false
````

### isInteger (obj)

Is the given argument an integer ?

```lua
M.isInteger(math.pi) -- => false
M.isInteger(1) -- => true
M.isInteger(-1) -- => true
````


**[[⬆]](#TOC)**


# <a name='not-work'>Not working with Luerl</a>

## Table functions

__NOTE: This list of table functions not work with Luerl.__

### eachi (t, f)
*Aliases: `forEachi`*.

Iterates only on integer keys in an array table. It returns value-key pairs.

```lua
M.eachi({4,2,1},print)

-- => 4 1
-- => 2 2
-- => 1 3
````

The given array can be sparse, or even have a hash-like part.

```lua
local t = {a = 1, b = 2, [0] = 1, [-1] = 6, 3, x = 4, 5}
M.eachi(t,print)

-- => 6 -1
-- => 1 0
-- => 3 1	
-- => 5 2
````

### adjust (t, key, f)

Adjusts the value at a given key using a function or a value. In case `f` is a function, it should be prototyped `f(v)`. 
It does not mutate the given table, but rather returns a new array.

```lua
local t = {1,2,3}
M.adjust(t, 2, math.sin) -- => {1, 0.90929, 3}

local v = {x = 1}
 M.adjust(t, 'x', 4) -- => {x = 4}
````

In case the given `key` does not exist in `t`, it throws an error.


### allEqual (t [, comp])
*Aliases: `alleq`*.

Checks if all values in a collection are equal. Uses `M.isEqual` by default to compare values.

```lua
M.allEqual({1,1,1,1,1}, comp) -- => true
M.allEqual({1,1,2,1,1}, comp) -- => false

local t1 = {1, 2, {3}}
local t2 = {1, 2, {3}}
M.allEqual({t1, t2}) -- => true
````

Can take an optional `comp` function which will be used to compare values.

```lua
local t1 = {x = 1, y = 0}
local t2 = {x = 1, y = 0}
local t3 = {x = 1, y = 2}
local t4 = {x = 1, y = 2}
local function compx(a, b) return a.x == b.x end
local function compy(a, b) return a.y == b.y end

M.allEqual({t1, t2}, compx) -- => true
M.allEqual({t1, t2}, compy) -- => true
M.allEqual({t3, t4}, compx) -- => true
M.allEqual({t3, t4}, compy) -- => true
M.allEqual({t1, t2, t3, t4}, compx) -- => true
M.allEqual({t1, t2, t3, t4}, compy) -- => false
````

### mapi (t, f)

Executes a function on each value in a given array.

```lua
M.mapi({1,2,3},function(v) 
  return v+10 
end) -- => "{11,12,13}"
````

It only works for the array-part of the given table.

```lua
M.map({a = 1, 2, 3, 4, 5},function(v, k) 
  return k..v 
end) -- => "{'12','23','34','45'}"
````

### best (t, f)

Returns the best value passing a selector function. Acts as a special case of `reduce`, using the first value in `t` as 
an initial state. It thens folds the given table, testing each of its values `v` and selecting the value passing the 
call `f(state,v)` every time.

```lua
local words = {'Lua', 'Programming', 'Language'}
M.best(words, function(a,b) return #a > #b end) -- => 'Programming'
M.best(words, function(a,b) return #a < #b end) -- => 'Lua'
````

### reduceBy (t, f, pred [, state = next(t)])

Reduces a table considering only values matching a predicate.
For example,let us define a set of values.

```lua
local val = {-1, 8, 0, -6, 3, -1, 7, 1, -9}
````
And a reduction function which will add up values.

```lua
local function add(a,b) return a+b end
````
 
We can also define some predicate functions.

```lua
-- predicate for negative values
local function neg(v) return v<=0 end

-- predicate for positive values
local function pos(v) return v>=0 end
````

Then we can perform reduction considering only negative or positive values :

```lua
M.reduceBy(val, add, neg) -- => -17
M.reduceBy(val, add, pos) -- => 19
````

An initial state can be passed in.

```lua
M.reduceBy(val, add, neg, 17) -- => 0
M.reduceBy(val, add, pos, -19) -- => 0
````

### sortedk (t [, comp])

Iterates on values with respect to key order. Keys are sorted using `comp` function which defaults to `math.min`. 
It returns upon each call a `key, value` pair.

```lua 
local tbl = {}; tbl[3] = 5 ; tbl[2] = 6; tbl[5] = 8; tbl[4] = 10; tbl[1] = 12
for k, v in M.sortedk(tbl) do print(k, v) end

-- => 1	12
-- => 2	6
-- => 3	5
-- => 4	10
-- => 5	8

local function comp(a,b) return a > b end
for k, v in M.sortedk(tbl, comp) do print(k, v) end

-- => 5	8
-- => 4	10
-- => 3	5
-- => 2	6
-- => 1	12
````

### sortedv (t [, comp])

Iterates on values with respect to key order. Keys are sorted using `comp` function which defaults to `math.min`. 
It returns upon each call a `key, value` pair.

```lua 
local tbl = {}; tbl[3] = 5 ; tbl[2] = 6; tbl[5] = 8; tbl[4] = 10; tbl[1] = 12
for k, v in M.sortedv(tbl) do print(k, v) end

-- => 3	5
-- => 2	6
-- => 5	8
-- => 4	10
-- => 1	12

local function comp(a,b) return a > b end
for k, v in M.sortedv(tbl, comp) do print(k, v) end

-- => 1	12
-- => 4	10
-- => 5	8
-- => 2	6
-- => 3	5
````

## Array functions

__NOTE: This list of array functions not work with Luerl.__

### nsorted (array [, n = 1[, comp]])

Returns the n-top values satisfying a predicate. It takes a comparison function `comp` used to sort array values, 
and then picks the top n-values. It leaves the original array untouched.

```lua
local function comp(a,b) return a > b end
M.nsorted(array,5, comp) -- => {5,4,3,2,1}
````

`n` defaults to 1 and `comp` defaults to the `<` operator.

```lua
local array = M.range(1,20)
M.nsorted(array) -- => {1}
````

### zeros (n)

Returns an array of `n` zeros.

```lua 
M.zeros(4) -- => {0,0,0,0}
````

### ones (n)

Returns an array of `n` 1's.

```lua 
M.ones(3) -- => {1,1,1}
````

### vector (value, n)

Returns an array of `n` times a given value.

```lua 
M.vector(10, 4) -- => {10,10,10,10}
````

### prepend (array, ...)

Adds given values at the top of an array, preserving the order at which elements are passed-in.

```lua
local array = {'old_val'}
M.prepend(array,1,2,3,4) -- => "{1,2,3,4,'old_val'}"
````

### chunk (array [, f])

Iterates over an array aggregating consecutive values in subsets tables, on the basis of the return value of `f(v, k, ...)`. Consecutive elements which return the same value are chunked together.

```lua
local t = {1,5,2,4,3,3,4}
M.chunk(t, function(v) return v%2==0 end) -- => "{{1,5},{2,4},{3,3},{4}}"
````

If not given, `f` defaults to `identity`.

```lua
local t = {1,5,2,4,3,3,4}
M.chunk(t) -- => "{{1},{5},{2},{4},{3,3},{4}}"
````

### disjoint (...)

Checks if all passed in arrays are disjoint.

```lua
local A = {'a'}
local B = {'a',1,3}
local C = {3,10,2}

M.disjoint(A,B) -- => false
M.disjoint(A,C) -- => true
M.disjoint(B,C) -- => false
````

### duplicates (array)

Returns an array list of all duplicates in array.

```lua
M.duplicates({1,2,3,3,8,8,3,2,4}) -- => {2,3,8}
````

### zipWith (f, ...)
*Aliases: `transposeWith`*.
      
Merges values using a given function. Only values indexed with the same key in the given arrays are merged in the same subset.
Function `f` is used to combine values.

```lua
local names = {'Bob','Alice','James'}; local ages = {22, 23, 25}
local function introduce(name, age) return 'I am '..name..' and I am '..age..' years old.' end
local t = M.zipWith(introduce,names,ages)
-- => {
-- =>  'I am Bob and I am 22 years old.'
-- =>  'I am Alice and I am 23 years old.'
-- =>  'I am James and I am 25 years old.'
-- => }
````

### powerset (array)

Returns the powerset of an array.

```lua
M.powerset {1,2,3} -- => "{{1},{2},{3},{1,2},{2,3},{1,2,3}}"
````

### partition (array [, n = 1 [, pad]])

*Aliases: `part`*.

Returns an iterator function for partitions of a given array.

```lua
local t = {1,2,3,4,5,6}
for p in M.partition(t,2) do
  print(table.concat(p, ','))
end

-- => 1,2
-- => 3,4
-- => 5,6

local t = {1,2,3,4,5,6}
for p in M.partition(t,4) do
  print(table.concat(p, ','))
end

-- => 1,2,3,4
-- => 5,6
````

In case the last partition has less elements than desired, a 3rd argument can be supplied to adjust the partition size.

```lua
local t = {1,2,3,4,5,6}
for p in M.partition(t,4,0) do
  print(table.concat(p, ','))
end

-- => 1,2,3,4
-- => 5,6,0,0
````

### overlapping (array [, n = 2 [, pad]])

Returns an iterator function which provides overlapping subsequences of a given array.

```lua
local t = {1,2,3,4,5,6,7}
for p in M.overlapping(t,3) do
	print(table.concat(p,','))
end

-- => 1,2,3
-- => 3,4,5
-- => 5,6,7

for p in M.overlapping(t,4) do
	print(table.concat(p,','))
end

-- => 1,2,3,4
-- => 4,5,6,7

for p in M.overlapping(t,5) do
	print(table.concat(p,','))
end

-- => 1,2,3,4,5
-- => 5,6,7
````

In case the last subsequence wil not match the exact desired length, it can be adjusted with a 3rd argument `pad`.

```lua
local t = {1,2,3,4,5,6,7}
for p in M.overlapping(t,5,0) do
	print(table.concat(p,','))
end

-- => 1,2,3,4,5
-- => 5,6,7,0,0
````

### aperture (array [, n = 2])
*Aliases: `sliding`*.

Returns an iterator function which provides sliding partitions of a given array.

```lua
local t = {1,2,3,4,5}
for p in M.aperture(t,4) do
  print(table.concat(p,','))
end

-- => 1,2,3,4
-- => 2,3,4,5

for p in M.aperture(t,3) do
  print(table.concat(p,','))
end

-- => 1,2,3
-- => 2,3,4
-- => 3,4,5
````

### pairwise (array)

Iterator returning sliding pairs of an array.

```lua
local t = M.range(5)
for p in pairwise(t) do
  print(table.concat(p,','))
end

-- => 1,2
-- => 2,3
-- => 3,4
-- => 4,5
````

### permutation (array)
*Aliases: `perm`*.

Returns an iterator function for permutations of a given array.

```lua
t = {'a','b','c'}
for p in M.permutation(t) do
  print(table.concat(p))
end

-- => 'bca'
-- => 'cba'
-- => 'cab'
-- => 'acb'
-- => 'bac'
-- => 'abc'
````

### xprod (array, array2)

Returns all possible pairs built from given arrays.

```lua
local t = M.xprod({1,2},{'a','b'})
-- => {{1,'a'},{1,'b'},{2,'a'},{2,'b'}}
````

### xpairs (value, array)

Creates pairs from value and array. Value is always prepended to the pair.

```lua
local t = M.xpairs(1, {1, 2, 3})
-- => {{1,1},{1,2},{1,3}}
````

### xpairsRight (value, array)

Creates pairs from value and array. Value is always appended as the last item to the pair.

```lua
local t = M.xpairsRight(1, {1, 2, 3})
-- => {{1,1},{2,1},{3,1}}
````

### sum (array)

Returns the sum of array values.

```lua
M.sum({1,2,3,4,5}) -- => 15
````

### product (array)

Returns the product of array values.

```lua
M.product({1,2,3,4,5}) -- => 120
````

### mean (array)

Returns the mean of array values.

```lua
M.mean({1,2,3,4,5}) -- => 3
````

### median (array)

Returns the median of array values.

```lua
M.median({1,2,3,4,5}) -- => 3
M.median({1,2,3,4}) -- => 2.5
````

## Utility functions

__NOTE: This list of utility functions not work with Luerl.__

### call (f [, ...])

Calls `f` with the supplied arguments. Returns the results of `f(...)`.

```lua
M.call(math.pow, 2, 3) -- => 8
M.call(string.len, 'hello' ) -- => 5
M.call(table.concat, {1,2,3,4,5}, ',', 2, 4) -- => {2,3,4}
````

### applySpec (specs)

Returns a function which applies `specs` on args. This function will produce an object having the same structure than `specs` 
by mapping each property to the result of calling its associated function with the supplied arguments.

```lua
local stats = M.applySpec({
  min = function(...) return math.min(...) end,
  max = function(...) return math.max(...) end,
})

stats(5,4,10,1,8) -- => {min = 1, max = 10}
````

### thread (value [, ...])

Threads `value` through a series of functions.

```lua
local function inc(x) return x + 1 end
local function double(x) return 2 * x end
local function square(x) return x * x end
M.thread(2, inc, double, square) -- => 36
M.thread(3, double, inc, square) -- => 49
M.thread(4, square, double, inc) -- => 33
M.thread(5, square, inc, double) -- => 52
````

If a function expects more than one args, it can be specified using an array list, 
where the first item is the function and the following are the remaining args neeeded. 

```lua
local function inc(x) return x + 1 end
local function add(x, y) return x * y end
local function pow(x, y) return x ^ y end
M.thread(2, inc, {add, 3}, {pow, 2}) -- => 36
M.thread(2, {add, 4}, inc, {pow, 2}) -- => 49
````

### threadRight (value [, ...])

Threads `value` through a series of functions. If a function expects more than one args, 
it can be specified using an array list, where the first item is the function and the following are 
the remaining args neeeded. The value is used as the last input.

```lua
local function inc(x) return x + 1 end
local function add(x, y) return x * y end
local function pow(x, y) return x ^ y end
M.threadRight(2, inc, {add, 3}, {pow, 2}) -- => 64
M.threadRight(2, {add, 4}, inc, {pow, 2}) -- => 128
````

### dispatch (...)

Returns a dispatching function. When called with arguments, this function invokes each of its functions 
in the passed-in order and returns the results of the first non-nil evaluation.

```lua
local f = M.dispatch(
  function() return nil end,
  function (v) return v+1 end, 
  function (v) return 2*v end
)
f(5) -- => 6
f(7) -- => 8
````

### unfold (f, seed)

Builds a list from a seed value. Accepts an iterator function, which returns either nil to stop iteration or two values : the value to add to the list of results and the seed to be used in the next call to the iterator function.

```lua
local function f(v)
  if v < 100 then return v, v * 2 end
end
local t = M.unfold(f, 10) -- => {10,20,40,80}
````

### times (iter [, n])

Calls a given function `n` times.

```lua
local f = ('Lua programming'):gmatch('.')
M.times(f, 3) -- => {'L','u','a'}
````

### bind2 (f, v)

Binds a value to be the second argument to a function.

```lua
local last2 = M.bind(M.last,2)
last2({1,2,3,4,5,6}) -- => {5,6}
````

### bindall (obj, ...)

Binds methods to object. As such, when calling any of these methods, they will receive object as a first argument.

```lua
local window = {
	setPos = function(w,x,y) w.x, w.y = x, y end, 
	setName = function(w,name) w.name = name end,
	getName = function(w) return w.name end,
}
window = M.bindall(window, 'setPos', 'setName', 'getName')
window.setPos(10,15)
print(window.x, window.y) -- => 10,15

window.setName('fooApp')
print(window.name) -- => 'fooApp'

print(window.getName()) -- => 'fooApp'
````

### cond (conds)

Returns a function which iterate over an array list of conditions. It invokes each predicate, passing it given values. It returns the value of the corresponding function of the first predicate to return a non-nil value

```lua
local multipleOf = M.cond({
  {function(v) return v%2==0 end, function(v) return v..' is multiple of 2' end},
  {function(v) return v%3==0 end, function(v) return v..' is multiple of 3' end},
  {function(v) return v%5==0 end, function(v) return v..' is multiple of 5' end},
  {function() return true end, function(v) return 'could not find an answer for '..v end}
})
for i = 15, 20 do
  print(multipleOf(i))
end

-- => 15 is multiple of 3
-- => 16 is multiple of 2
-- => could not find an answer for 17
-- => 18 is multiple of 2
-- => could not find an answer for 19
-- => 20 is multiple of 2
````

### both (...)

Returns a validation function. Given a set of functions, the validation function 
evaluates to `true` only when all its funcs returns `true`.

```lua
local f = M.both(
	function(x) return x > 0 end,
	function(x) return x < 10 end,
	function(x) return x % 2 == 0 end
)
f(2) -- => true
f(8) -- => true
f(9) -- => false
````

### either (...)

Returns a validation function. Given a set of functions, the validation function 
evaluates to `true` when one of its funcs returns `true`.

```lua
local f = M.either(
	function(x) return x > 0 end,
	function(x) return x % 2 == 0 end
)
f(0) -- => true
f(-3) -- => false
````

### neither (...)

Returns a validation function. Given a set of functions, the validation function 
evaluates to `true` when neither of its funcs returns `true`.

```lua
local f = M.neither(
	function(x) return x > 10 end,
	function(x) return x % 2 == 0 end
)
f(12) -- => false
f(8) -- => false
f(7) -- => true
````

### iterator (f, value [, n])

*Aliases: `iter`*.

Returns an iterator function which constinuously applies a function `f` onto an input `value`.
For example, let us go through the powers of two using `iterator`.

```lua
local function po2(x) return x*2 end
local function iter_po2 = M.iterator(po2, 1)
iter_po2() -- => 2
iter_po2() -- => 4
iter_po2() -- => 8
````

if `n` is supplied, it will run at maximum `n` times.

```lua
local function po2(x) return x*2 end
local function iter_po2 = M.iterator(po2, 1, 3)
iter_po2() -- => 2
iter_po2() -- => 4
iter_po2() -- => 8
iter_po2() -- => nil
````

### skip (iter [, n = 1])

Consumes the first `n` values of a iterator then returns it.

```lua
local w = "hello"
local char = string.gmatch(w,'.')
local iter = M.skip(char, 3)
for w in iter do print(w) end -- => 'l', 'o'
````

`n` defaults to 1 when not given.

```lua
local w = "hello"
local char = string.gmatch(w,'.')
local iter = M.skip(char)
for w in iter do print(w) end -- => 'e', 'l', 'l', 'o'
````

### tabulate (...)

Iterates a given iterator function and returns its values packed in an array.

```lua
local text = 'letters'
local chars = string.gmatch(text, '.')
M.tabulate(chars) -- => {'l','e','t','t','e','r','s'}
````

### iterlen (...)

Returns the length of an iterator.

```lua
local text = 'letters'
local chars = string.gmatch(text, '.')
M.iterlen(chars) -- => 7
````

It consumes the iterator itself.

```lua
local text = 'lua'
local chars = string.gmatch(text, '.')
M.iterlen(chars) -- => 3
chars() -- => nil
````

### castArray (value)

Casts the passed-in value to an array containing the value itself.

```lua
M.castArray(true) -- => {true}
M.castArray(2) -- => {2}
````

It leaves the given value untouched in case it is already a table.

```lua
local t = {1}
print(M.castArray(t) == t) -- => true
````

### nthArg (n)

Returns a function that gets the nth argument. 

```lua
local f = M.nthArg(3)
f('a','b','c') -- => 'c'
````

If n is negative, the nth argument from the end is returned.

```lua
local f = M.nthArg(-2)
f('a','b','c') -- => 'b'
````

### unary (f)

Returns a function which accepts up to one argument. It ignores any additional arguments. 

```lua
local f = M.unary(function (...) return ... end)
f('a') - ==> 'a'
f('a','b','c') -- => 'a'
````

### ary (f [, n = 1])
*Aliases: `nAry`*.

Returns a function which accepts up to `n` args. It ignores any additional arguments.

```lua
local f = M.ary(function (...) return ... end, 2)
f(1,2) - ==> 1,2
f(1,2,3,4) -- => 1,2
````

If `n` is not given, it defaults to `1`.

```lua
local f = M.unary(function (...) return ... end)
f('a','b','c') -- => 'a'
````

### noarg (f)

Returns a function with an arity of 0. The new function ignores any arguments passed to it.

```lua
local f = M.noarg(function (x) return x or 'default' end)
f(1) -- => 'default'
f(function() end, 3) -- => 'default'
````

### rearg (f, indexes)

Returns a function which runs with arguments arranged according to given `indexes`.

```lua
local f = M.rearg(function (...) return ... end, {5,4,3,2,1})
f('a','b','c','d','e') -- => 'e','d','c','b','a'
````

### converge (f, g, h)

Converges two functions into one.

```lua
local function pow2(x) return x*x end
local function pow3(x) return x*x*x end
local function sum(a,b) return a+b end
local poly = M.converge(sum, pow2, pow3)
poly(5) -- => 150 (ie. 5*5 + 5*5*5)
````

### curry (f [, n_args = 2])

Curries a function. If the given function `f` takes multiple arguments, it returns another version of `f` that takes a single argument 
(the first of the arguments to the original function) and returns a new function that takes the remainder of the arguments and returns the result.

```lua
local function sumOf3args(x,y,z) return x + y + z end
local curried_sumOf3args = M.curry(sumOf3args, 3)
sumOf3args(1)(2)(3)) -- => 6
sumOf3args(0)(6)(9)) -- => 15
````

`n_args` defaults to 2.

```lua
local function product(x,y) return x * y end
local curried_product = M.curry(product)
curried_product(5)(4) -- => 20
curried_product(3)(-5) -- => -15
curried_product(0)(1) -- => 0
````

## Object functions

__NOTE: This list of object functions not work with Luerl.__

### path (obj, ...)

Returns the value at a given path in an object.

```lua
local entity = {
  pos = {x = 1, y = 2},
  engine = {
    left = {status = 'active', damage = 5},
    right = {status = 'off', damage = 10}
  },
  boost = false
}

M.path(entity,'pos','x') -- => 1
M.path(entity,'pos','y') -- => 2
M.path(entity,'engine','left','status') -- => 'active'
M.path(entity,'engine','right','damage') -- => 10
M.path(entity,'boost') -- => false
````

### spreadPath (obj, ...)

Spreads object under property path onto provided object. It is similar to `flattenPath`, but removes object under the property path.

```lua
local obj = {a = 1, b = 2, c = {d = 3, e = 4, f = {g = 5}}}
M.spreadPath(obj, 'c', 'f')
-- => {a = 1, b = 2, d = 3, e = 4, g = 5, c = {f = {}}}
````

### flattenPath (obj, ...)

Flattens object under property path onto provided object. It is similar to `spreadPath`, but preserves object under the property path.

```lua
local obj = {a = 1, b = 2, c = {d = 3, e = 4, f = {g = 5}}}
M.spreadPath(obj, 'c', 'f')
-- => {a = 1, b = 2, d = 3, e = 4, g = 5, c = {d = 3, e = 4, f = {g = 5}}}
````

### kvpairs (obj)

Converts an object to an array-list of key-value pairs.

```lua
local obj = {x = 1, y = 2, z = 3}
M.each(M.kvpairs(obj), function(v,k)
	print(k, table.concat(v,','))	
end)

-- => 1	y,2
-- => 2	x,1
-- => 3	z,3
````

### has (obj, key)

Checks if an object has a given attribute.

```lua
M.has(_,'has') -- => true
M.has(coroutine,'resume') -- => true
M.has(math,'random') -- => true
````

### type (obj)

Extends Lua's `type` function. It returns the type of the given object and also recognises 'file' userdata

```lua
M.type('string') -- => 'string'
M.type(table) -- => 'table'
M.type(function() end) -- => 'function'
M.type(io.open('f','w')) -- => 'file'
````

### isNumber (obj)

Is the given argument a number ?

```lua
M.isNumber(math.pi) -- => true
M.isNumber(math.huge) -- => true
M.isNumber(0/0) -- => true
M.isNumber() -- => false
````

### isNaN (obj)

Is the given argument NaN ?

```lua
M.isNaN(1) -- => false
M.isNaN(0/0) -- => true
````

### isFinite (obj)

Is the given argument a finite number ?

```lua
M.isFinite(99e99) -- => true
M.isFinite(math.pi) -- => true
M.isFinite(math.huge) -- => false
M.isFinite(1/0) -- => false
M.isFinite(0/0) -- => false
````

## Chaining

__Moses offers chaining but this feature is not working with Luerl.__

Method chaining (also known as name parameter idiom), is a technique for invoking consecutively method calls in object-oriented style. Each method returns an object, and method calls are chained together.

```lua

local t = {1,2,3}
print(_(t):value() == t) -- => true
````

## Import

__NOTE: This library feature not work with Luerl.__

All library functions can be imported in a conext using `import` into a specified context.
```lua

local context = {}
M.import(context)

context.each({1,2,3},print)

-- => 1 1
-- => 2 2
-- => 3 3
````

When no `context` was provided, it defaults to the current environment, `_ENV` or `_G`.

## License

This work is under [MIT-LICENSE](http://www.opensource.org/licenses/mit-license.php)<br/>
Copyright (c) 2012-2021 Roland Yonaba. <br/>
See [LICENSE](https://github.com/Yonaba/Moses/blob/master/LICENSE).
