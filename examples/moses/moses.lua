--- Utility-belt library for functional programming in Lua ([source](http://github.com/Yonaba/Moses))
-- @author [Roland Yonaba](http://github.com/Yonaba)
-- @copyright 2012-2018
-- @license [MIT](http://www.opensource.org/licenses/mit-license.php)
-- @release 2.1.0
-- @module moses
-- @set sort=true

local _MODULEVERSION = '2.1.0'

-- Internalisation
local next, type, pcall          = next, type, pcall
local setmetatable, getmetatable = setmetatable, getmetatable
local t_insert, t_sort           = table.insert, table.sort
local t_remove,t_concat          = table.remove, table.concat
local randomseed, random, huge   = math.randomseed, math.random, math.huge
local floor, max, min, ceil      = math.floor, math.max, math.min, math.ceil
local wrap                       = coroutine.wrap
local yield                      = coroutine.yield
local rawget                     = rawget
local unpack                     = table.unpack or unpack
local pairs,ipairs               = pairs,ipairs
local error                      = error
local clock                      = os and os.clock or nil
local M                          = {}


-- ======== Private helpers

local function f_max(a,b) return a>b end
local function f_min(a,b) return a<b end

local function count(t)  -- raw count of items in an map-table
  local i = 0
    for k,v in pairs(t) do i = i + 1 end
  return i
end

local function extract(list,comp,transform,...) -- extracts value from a list
  transform = transform or M.identity
  local _ans  
  for k,v in pairs(list) do
    if not _ans then _ans = transform(v,...)
    else
      local val = transform(v,...)
      _ans = comp(_ans,val) and _ans or val
    end
  end
  return _ans
end

local function partgen(t, n, f, pad) -- generates array partitions
  for i = 0, #t, n do
    local s = M.slice(t, i+1, i+n)
    if #s>0 then 
			while (#s < n and pad) do s[#s+1] = pad end			
			f(s)
		end
  end
end

local function partgen2(t, n, f, pad) -- generates overlapping array partitions
  for i = 0, #t, n-1 do
    local s = M.slice(t, i+1, i+n)
    if #s>0 and i+1<#t then 
			while (#s < n and pad) do s[#s+1] = pad end
			f(s)
		end
  end
end

local function partgen3(t, n, f, pad) -- generates sliding array partitions
  for i = 0, #t, 1 do
    local s = M.slice(t, i+1, i+n)
    if #s>0 and i+n<=#t then 
			while (#s < n and pad) do s[#s+1] = pad end
			f(s)
		end
  end
end

local function permgen(t, n, f) -- taken from PiL: http://www.lua.org/pil/9.3.html
  if n == 0 then f(t) end
  for i = 1,n do
    t[n], t[i] = t[i], t[n]
    permgen(t, n-1, f)
    t[n], t[i] = t[i], t[n]
  end
end

local function signum(a) return a>=0 and 1 or -1 end

-- Internal counter for unique ids generation
local unique_id_counter = -1

--- Operator functions
-- @section Operator functions

M.operator = {}
--- Returns a + b. <em>Aliased as `op.add`</em>.
-- @name operator.add
-- @param a a value
-- @param b a value
-- @return a + b
M.operator.add = function(a,b) return a + b end

--- Returns a - b. <em>Aliased as `op.sub`</em>.
-- @name operator.sub
-- @param a a value
-- @param b a value
-- @return a - b
M.operator.sub = function(a,b) return a - b end

--- Returns a * b. <em>Aliased as `op.mul`</em>.
-- @name operator.mul
-- @param a a value
-- @param b a value
-- @return a * b
M.operator.mul = function(a,b) return a * b end

--- Returns a / b. <em>Aliased as `op.div`</em>.
-- @name operator.div
-- @param a a value
-- @param b a value
-- @return a / b
M.operator.div = function(a,b) return a / b end

--- Returns a % b. <em>Aliased as `op.mod`</em>.
-- @name operator.mod
-- @param a a value
-- @param b a value
-- @return a % b
M.operator.mod = function(a,b) return a % b end

--- Returns a ^ b. <em>Aliased as `op.exp`, `op.pow`</em>.
-- @name operator.exp
-- @param a a value
-- @param b a value
-- @return a ^ b
M.operator.exp = function(a,b) return a ^ b end
M.operator.pow = M.operator.exp

--- Returns -a. <em>Aliased as `op.unm`, `op.neg`</em>.
-- @name operator.unm
-- @param a a value
-- @return -a
M.operator.unm = function(a) return -a end
M.operator.neg = M.operator.unm

--- Performs floor division (//) between `a` and `b`. It rounds the quotient towards minus infinity.
-- <em>Aliased as `op.floordiv`</em>.
-- @name operator.floordiv
-- @param a a value
-- @param b a value
-- @return a // b
M.operator.floordiv = function(a, b) return floor(a/b) end 

--- Performs integer division between `a` and `b`. <em>Aliased as `op.intdiv`</em>.
-- @name operator.intdiv
-- @param a a value
-- @param b a value
-- @return a / b
M.operator.intdiv = function(a,b)
  return a>=0 and floor(a/b) or ceil(a/b) 
end

--- Checks if a equals b. <em>Aliased as `op.eq`</em>.
-- @name operator.eq
-- @param a a value
-- @param b a value
-- @return a == b
M.operator.eq = function(a,b) return a == b end

--- Checks if a not equals b. <em>Aliased as `op.neq`</em>.
-- @name operator.neq
-- @param a a value
-- @param b a value
-- @return a ~= b
M.operator.neq = function(a,b) return a ~= b end

--- Checks if a is strictly less than b. <em>Aliased as `op.lt`</em>.
-- @name operator.lt
-- @param a a value
-- @param b a value
-- @return a < b
M.operator.lt = function(a,b) return a < b end

--- Checks if a is strictly greater than b. <em>Aliased as `op.gt`</em>.
-- @name operator.gt
-- @param a a value
-- @param b a value
-- @return a > b
M.operator.gt = function(a,b) return a > b end

--- Checks if a is less or equal to b. <em>Aliased as `op.le`</em>.
-- @name operator.le
-- @param a a value
-- @param b a value
-- @return a <= b
M.operator.le = function(a,b) return a <= b end

--- Checks if a is greater or equal to b. <em>Aliased as `op.ge`</em>.
-- @name operator.ge
-- @param a a value
-- @param b a value
-- @return a >= b
M.operator.ge = function(a,b) return a >= b end

--- Returns logical a and b. <em>Aliased as `op.land`</em>.
-- @name operator.land
-- @param a a value
-- @param b a value
-- @return a and b
M.operator.land = function(a,b) return a and b end

--- Returns logical a or b. <em>Aliased as `op.lor`</em>.
-- @name operator.lor
-- @param a a value
-- @param b a value
-- @return a or b
M.operator.lor = function(a,b) return a or b end

--- Returns logical not a. <em>Aliased as `op.lnot`</em>.
-- @name operator.lnot
-- @param a a value
-- @return not a
M.operator.lnot = function(a) return not a end

--- Returns concatenation of a and b. <em>Aliased as `op.concat`</em>.
-- @name operator.concat
-- @param a a value
-- @param b a value
-- @return a .. b
M.operator.concat = function(a,b) return a..b end

--- Returns the length of a. <em>Aliased as `op.len`</em>.
-- @name operator.length
-- @param a a value
-- @return #a
M.operator.length = function(a) return #a end
M.operator.len = M.operator.length

--- Table functions
-- @section Table functions

--- Clears a table. All its values become nil.
-- @name clear
-- @param t a table
-- @return the given table, cleared.
function M.clear(t)
	for k in pairs(t) do t[k] = nil end
	return t
end



--- Iterates on key-value pairs, calling `f (v, k)` at every step.
-- <br/><em>Aliased as `forEach`</em>.
-- @name each
-- @param t a table
-- @param f a function, prototyped as `f (v, k)`
-- @see eachi
function M.each(t, f)
  for index,value in pairs(t) do
    f(value, index)
  end
end

--- Iterates on integer key-value pairs, calling `f(v, k)` every step. 
-- Only applies to values located at integer keys. The table can be a sparse array. 
-- Iteration will start from the lowest integer key found to the highest one.
-- <br/><em>Aliased as `forEachi`</em>.
-- @name eachi
-- @param t a table
-- @param f a function, prototyped as `f (v, k)`
-- @see each
function M.eachi(t, f)
  local lkeys = M.sort(M.select(M.keys(t), M.isInteger))
  for k, key in ipairs(lkeys) do
    f(t[key], key)
  end
end

--- Collects values at given keys and return them wrapped in an array.
-- @name at
-- @param t a table
-- @param ... A variable number of keys to collect values
-- @return an array-list of values
function M.at(t, ...)
  local values = {}
  for i, key in ipairs({...}) do values[#values+1] = t[key] end
  return values
end

--- Adjusts the value at a given key using a function or a value. In case `f` is a function, 
-- it should be prototyped `f(v)`. It does not mutate the given table, but rather
-- returns a new array. In case the given `key` does not exist in `t`, it throws an error.
-- @param t a table
-- @param key a key
-- @param f a function, prototyped as `f(v)` or a value
function M.adjust(t, key, f)
  if (t[key] == nil) then error("key not existing in table") end
  local _t = M.clone(t)
  _t[key] = type(f) == 'function' and f(_t[key]) or f
  return _t
end

--- Counts occurrences of a given value in a table. Uses @{isEqual} to compare values.
-- @name count
-- @param t a table
-- @param[opt] val a value to be searched in the table. If not given, the @{size} of the table will be returned
-- @return the count of occurrences of the given value
-- @see countf
-- @see size
function M.count(t, val)
  if val == nil then return M.size(t) end
  local count = 0
  for k, v in pairs(t) do
    if M.isEqual(v, val) then count = count + 1 end
  end
  return count
end

--- Counts the number of values passing a predicate test. Same as @{count}, but uses an iterator. 
-- Returns the count for values passing the test `f (v, k)`
-- @name countf
-- @param t a table
-- @param f an iterator function, prototyped as `f (v, k)`
-- @return the count of values validating the predicate
-- @see count
-- @see size
function M.countf(t, f)
  local count = 0
  for k, v in pairs(t) do
    if f(v, k) then count = count + 1 end
  end
  return count
end

--- Checks if all values in a collection are equal. Uses an optional `comp` function which is used
-- to compare values and defaults to @{isEqual} when not given.
-- <br/><em>Aliased as `alleq`</em>.
-- @name allEqual
-- @param t a table
-- @param[opt] comp a comparison function. Defaults to `isEqual`
-- @return `true` when all values in `t` are equal, `false` otherwise.
-- @see isEqual
function M.allEqual(t, comp)
  local k, pivot = next(t)
  for k, v in pairs(t) do
    if comp then 
      if not comp(pivot, v) then return false end
    else
      if not M.isEqual(pivot, v) then return false end
    end
  end
  return true
end

--- Loops `n` times through a table. In case `n` is omitted, it will loop forever.
-- In case `n` is lower or equal to 0, it returns an empty function.
-- <br/><em>Aliased as `loop`</em>.
-- @name cycle
-- @param t a table
-- @param[opt] n the number of loops
-- @return an iterator function yielding value-key pairs from the passed-in table.
function M.cycle(t, n)
  n = n or 1
  if n<=0 then return M.noop end
  local k, fk
  local i = 0
  while true do
    return function()
      k = k and next(t,k) or next(t)
      fk = not fk and k or fk
      if n then
        i = (k==fk) and i+1 or i
        if i > n then
          return
        end
      end
      return t[k], k
    end
  end
end

--- Maps `f (v, k)` on value-key pairs, collects and returns the results. 
-- Uses `pairs` to iterate over elements in `t`.
-- <br/><em>Aliased as `collect`</em>.
-- @name map
-- @param t a table
-- @param f  an iterator function, prototyped as `f (v, k)`
-- @return a table of results
-- @see mapi
function M.map(t, f)
  local _t = {}
  for index,value in pairs(t) do
    local k, kv, v = index, f(value, index)
    _t[v and kv or k] = v or kv
  end
  return _t
end

--- Maps `f (v, k)` on value-key pairs, collects and returns the results. 
-- Uses `ipairs` to iterate over elements in `t`.
-- @name mapi
-- @param t a table
-- @param f  an iterator function, prototyped as `f (v, k)`
-- @return a table of results
-- @see map
function M.mapi(t, f)
  local _t = {}
  for index,value in ipairs(t) do
    local k, kv, v = index, f(value, index)
    _t[v and kv or k] = v or kv
  end
  return _t
end

--- Reduces a table, left-to-right. Folds the table from the first element to the last element
-- to a single value, using a given iterator and an initial state.
-- The iterator takes a state and a value and returns a new state.
-- <br/><em>Aliased as `inject`, `foldl`</em>.
-- @name reduce
-- @param t a table
-- @param f an iterator function, prototyped as `f (state, value)`
-- @param[opt] state an initial state of reduction. Defaults to the first value in the table.
-- @return the final state of reduction
-- @see best
-- @see reduceRight
-- @see reduceBy
function M.reduce(t, f, state)
  for k,value in pairs(t) do
    if state == nil then state = value
    else state = f(state,value)
    end
  end
  return state
end

--- Returns the best value passing a selector function. Acts as a special case of
-- @{reduce}, using the first value in `t` as an initial state. It thens folds the given table,
-- testing each of its values `v` and selecting the value passing the call `f(state,v)` every time.
-- @name best
-- @param t a table
-- @param f an iterator function, prototyped as `f (state, value)`
-- @return the final state of reduction
-- @see reduce
-- @see reduceRight
-- @see reduceBy
function M.best(t, f)
  local _, state = next(t)
  for k,value in pairs(t) do
    if state == nil then state = value
    else state = f(state,value) and state or value
    end
  end
  return state
end

--- Reduces values in a table passing a given predicate. Folds the table left-to-right, considering
-- only values validating a given predicate.
-- @name reduceBy
-- @param t a table
-- @param f an iterator function, prototyped as `f (state, value)`
-- @param pred a predicate function `pred (v, k)` to select values to be considered for reduction
-- @param[opt] state an initial state of reduction. Defaults to the first value in the table of selected values.
-- @param[optchain] ... optional args to be passed to `pred`
-- @return the final state of reduction
-- @see reduce
-- @see best
-- @see reduceRight
function M.reduceBy(t, f, pred, state)
	return M.reduce(M.select(t, pred), f, state)
end

--- Reduces a table, right-to-left. Folds the table from the last element to the first element 
-- to single value, using a given iterator and an initial state.
-- The iterator takes a state and a value, and returns a new state.
-- <br/><em>Aliased as `injectr`, `foldr`</em>.
-- @name reduceRight
-- @param t a table
-- @param f an iterator function, prototyped as `f (state, value)`
-- @param[opt] state an initial state of reduction. Defaults to the last value in the table.
-- @return the final state of reduction
-- @see reduce
-- @see best
-- @see reduceBy
function M.reduceRight(t, f, state)
  return M.reduce(M.reverse(t),f,state)
end

--- Reduces a table while saving intermediate states. Folds the table left-to-right
-- using a given iterator and an initial state. The iterator takes a state and a value, 
-- and returns a new state. The result is an array of intermediate states.
-- <br/><em>Aliased as `mapr`</em>
-- @name mapReduce
-- @param t a table
-- @param f an iterator function, prototyped as `f (state, value)`
-- @param[opt] state an initial state of reduction. Defaults to the first value in the table.
-- @return an array of states
-- @see mapReduceRight
function M.mapReduce(t, f, state)
  local _t = {}
  for i,value in pairs(t) do
    _t[i] = not state and value or f(state,value)
    state = _t[i]
  end
  return _t
end

--- Reduces a table while saving intermediate states. Folds the table right-to-left
-- using a given iterator and an initial state. The iterator takes a state and a value, 
-- and returns a new state. The result is an array of intermediate states.
-- <br/><em>Aliased as `maprr`</em>
-- @name mapReduceRight
-- @param t a table
-- @param f an iterator function, prototyped as `f (state, value)`
-- @param[opt] state an initial state of reduction. Defaults to the last value in the table.
-- @return an array of states
-- @see mapReduce
function M.mapReduceRight(t, f, state)
  return M.mapReduce(M.reverse(t),f,state)
end

--- Performs a linear search for a value in a table. It does not work for nested tables.
-- The given value can be a function prototyped as `f (v, value)` which should return true when
-- any v in the table equals the value being searched. 
-- <br/><em>Aliased as `any`, `some`, `contains`</em>
-- @name include
-- @param t a table
-- @param value a value to search for
-- @return a boolean : `true` when found, `false` otherwise
-- @see detect
function M.include(t, value)
  local _iter = (type(value) == 'function') and value or M.isEqual
  for k,v in pairs(t) do
    if _iter(v,value) then return true end
  end
  return false
end

--- Performs a linear search for a value in a table. Returns the key of the value if found.
-- The given value can be a function prototyped as `f (v, value)` which should return true when
-- any v in the table equals the value being searched. This function is similar to @{find}, 
-- which is mostly meant to work with array.
-- @name detect
-- @param t a table
-- @param value a value to search for
-- @return the key of the value when found or __nil__
-- @see include
-- @see find
function M.detect(t, value)
  local _iter = (type(value) == 'function') and value or M.isEqual
  for key,arg in pairs(t) do
    if _iter(arg,value) then return key end
  end
end

--- Returns all values having specified keys `props`.
-- @name where
-- @param t a table
-- @param props a set of keys
-- @return an array of values from the passed-in table
-- @see findWhere
function M.where(t, props)
	local r = M.select(t, function(v)
		for key in pairs(props) do
			if v[key] ~= props[key] then return false end
		end
		return true
	end)
	return #r > 0 and r or nil
end

--- Returns the first value having specified keys `props`.
-- @name findWhere
-- @param t a table
-- @param props a set of keys
-- @return a value from the passed-in table
-- @see where
function M.findWhere(t, props)
  local index = M.detect(t, function(v)
    for key in pairs(props) do
      if props[key] ~= v[key] then return false end
    end
    return true
  end)
  return index and t[index]
end

--- Selects and returns values passing an iterator test.
-- <br/><em>Aliased as `filter`</em>.
-- @name select
-- @param t a table
-- @param f an iterator function, prototyped as `f (v, k)`
-- @return the selected values
-- @see reject
function M.select(t, f)
  local _t = {}
  for index,value in pairs(t) do
    if f(value,index) then _t[#_t+1] = value end
  end
  return _t
end

--- Clones a table while dropping values passing an iterator test.
-- <br/><em>Aliased as `discard`</em>
-- @name reject
-- @param t a table
-- @param f an iterator function, prototyped as `f (v, k)`
-- @return the remaining values
-- @see select
function M.reject(t, f)
  local _t = {}
  for index,value in pairs (t) do
    if not f(value,index) then _t[#_t+1] = value end
  end
  return _t
end

--- Checks if all values in a table are passing an iterator test.
-- <br/><em>Aliased as `every`</em>
-- @name all
-- @param t a table
-- @param f an iterator function, prototyped as `f (v, k)`
-- @return `true` if all values passes the predicate, `false` otherwise
function M.all(t, f)
  for index,value in pairs(t) do
    if not f(value,index) then return false end
  end
  return true
end

--- Invokes a method on each value in a table.
-- @name invoke
-- @param t a table
-- @param method a function, prototyped as `f (v, k)`
-- @return the result of the call `f (v, k)`
-- @see pluck
function M.invoke(t, method)
  return M.map(t, function(v, k)
    if (type(v) == 'table') then
      if v[method] then
        if M.isCallable(v[method]) then
          return v[method](v,k)
        else
          return v[method]
        end
      else
        if M.isCallable(method) then
          return method(v,k)
        end
      end
    elseif M.isCallable(method) then
      return method(v,k)
    end
  end)
end

--- Extracts values in a table having a given key.
-- @name pluck
-- @param t a table
-- @param key a key, will be used to index in each value: `value[key]`
-- @return an array of values having the given key
function M.pluck(t, key)
  local _t = {}
  for k, v in pairs(t) do
    if v[key] then _t[#_t+1] = v[key] end
  end
  return _t
end

--- Returns the max value in a collection. If a `transform` function is passed, it will
-- be used to evaluate values by which all objects will be sorted.
-- @name max
-- @param t a table
-- @param[opt] transform a transformation function, prototyped as `transform (v, k)`, defaults to @{identity}
-- @return the max value found
-- @see min
function M.max(t, transform)
  return extract(t, f_max, transform)
end

--- Returns the min value in a collection. If a `transform` function is passed, it will
-- be used to evaluate values by which all objects will be sorted.
-- @name min
-- @param t a table
-- @param[opt] transform a transformation function, prototyped as `transform (v, k)`, defaults to @{identity}
-- @return the min value found
-- @see max
function M.min(t, transform)
  return extract(t, f_min, transform)
end

--- Checks if two tables are the same. It compares if both tables features the same values,
-- but not necessarily at the same keys.
-- @name same
-- @param a a table
-- @param b another table
-- @return `true` or `false`
function M.same(a, b)
  return M.all(a, function(v) return M.include(b,v) end) 
     and M.all(b, function(v) return M.include(a,v) end)
end

--- Sorts a table, in-place. If a comparison function is given, it will be used to sort values.
-- @name sort
-- @param t a table
-- @param[opt] comp a comparison function prototyped as `comp (a, b)`, defaults to <tt><</tt> operator.
-- @return the given table, sorted.
-- @see sortBy
function M.sort(t, comp)
  t_sort(t, comp)
  return t
end

--- Iterates on values with respect to key order. Keys are sorted using `comp` function
-- which defaults to `math.min`. It returns upon each call a `key, value` pair.
-- @name sortedk
-- @param t a table 
-- @param[opt] comp a comparison function. Defaults to `<` operator
-- @return an iterator function 
-- @see sortedv 
function M.sortedk(t, comp)
  local keys = M.keys(t)
  t_sort(keys, comp)
  local i = 0
  return function ()
    i = i + 1
    return keys[i], t[keys[i]]
  end
end

--- Iterates on values with respect to values order. Values are sorted using `comp` function
-- which defaults to `math.min`. It returns upon each call a `key, value` pair.
-- @name sortedv
-- @param t a table 
-- @param[opt] comp a comparison function. Defaults to `<` operator
-- @return an iterator function 
-- @see sortedk
function M.sortedv(t, comp)
  local keys = M.keys(t)
  comp = comp or f_min
  t_sort(keys, function(a,b) return comp(t[a],t[b]) end)
  local i = 0
  return function ()
    i = i + 1
    return keys[i], t[keys[i]]
  end
end

--- Sorts a table in-place using a transform. Values are ranked in a custom order of the results of
-- running `transform (v)` on all values. `transform` may also be a string name property  sort by. 
-- `comp` is a comparison function.
-- @name sortBy
-- @param t a table
-- @param[opt] transform a `transform` function to sort elements prototyped as `transform (v)`. Defaults to @{identity}
-- @param[optchain] comp a comparison function, defaults to the `<` operator
-- @return a new array of sorted values
-- @see sort
function M.sortBy(t, transform, comp)
	local f = transform or M.identity
	if (type(transform) == 'string') then
		f = function(t) return t[transform] end
	end
	comp = comp or f_min	
	t_sort(t, function(a,b) return comp(f(a), f(b)) end)
	return t
end

--- Splits a table into subsets groups.
-- @name groupBy
-- @param t a table
-- @param iter an iterator function, prototyped as `iter (v, k)`
-- @return a table of subsets groups
function M.groupBy(t, iter)
  local _t = {}
  for k,v in pairs(t) do
    local _key = iter(v,k)
    if _t[_key] then _t[_key][#_t[_key]+1] = v
    else _t[_key] = {v}
    end
  end
  return _t
end

--- Groups values in a collection and counts them.
-- @name countBy
-- @param t a table
-- @param iter an iterator function, prototyped as `iter (v, k)`
-- @return a table of subsets groups names paired with their count
function M.countBy(t, iter)
  local stats = {}
  for i,v in pairs(t) do
    local key = iter(v,i)
    stats[key] = (stats[key] or 0)+1
  end
  return stats
end

--- Counts the number of values in a collection. If being passed more than one argument
-- it will return the count of all passed-in arguments.
-- @name size
-- @param[opt] ... Optional variable number of arguments
-- @return a count
-- @see count
-- @see countf
function M.size(...)
  local args = {...}
  local arg1 = args[1]
  return (type(arg1) == 'table') and count(args[1]) or count(args)
end

--- Checks if all the keys of `other` table exists in table `t`. It does not
-- compares values. The test is not commutative, i.e table `t` may contains keys
-- not existing in `other`.
-- @name containsKeys
-- @param t a table
-- @param other another table
-- @return `true` or `false`
-- @see sameKeys
function M.containsKeys(t, other)
  for key in pairs(other) do
    if not t[key] then return false end
  end
  return true
end

--- Checks if both given tables have the same keys. It does not compares values.
-- @name sameKeys
-- @param tA a table
-- @param tB another table
-- @return `true` or `false`
-- @see containsKeys
function M.sameKeys(tA, tB)
  for key in pairs(tA) do
    if not tB[key] then return false end
  end
  for key in pairs(tB) do
    if not tA[key] then return false end
  end
  return true
end

--- Array functions
-- @section Array functions

--- Samples `n` random values from an array. If `n` is not specified, returns a single element.
-- It uses internally @{shuffle} to shuffle the array before sampling values. If `seed` is passed,
-- it will be used for shuffling.
-- @name sample
-- @param array an array
-- @param[opt] n a number of elements to be sampled. Defaults to 1.
-- @param[optchain] seed an optional seed for shuffling 
-- @return an array of selected values
-- @see sampleProb
function M.sample(array, n, seed)
  n = n or 1    
  if n == 0 then return {} end
	if n == 1 then
		if seed then randomseed(seed) end
		return {array[random(1, #array)]}
	end
	return M.slice(M.shuffle(array, seed), 1, n)
end

--- Return elements from a sequence with a given probability. It considers each value independently. 
-- Providing a seed will result in deterministic sampling. Given the same seed it will return the same sample
-- every time.
-- @name sampleProb
-- @param array an array
-- @param prob a probability for each element in array to be selected
-- @param[opt] seed an optional seed for deterministic sampling
-- @return an array of selected values
-- @see sample
function M.sampleProb(array, prob, seed)
	if seed then randomseed(seed) end
  local t = {}
  for k, v in ipairs(array) do
    if random() < prob then t[#t+1] = v end
  end
	return t
end

--- Returns the n-top values satisfying a predicate. It takes a comparison function
-- `comp` used to sort array values, and then picks the top n-values. It leaves the original array untouched.
-- @name nsorted
-- @param array an array
-- @param[opt] n a number of values to retrieve. Defaults to 1.
-- @param[optchain] comp a comparison function. Defaults to `<` operator.
-- @return an array of top n values
function M.nsorted(array, n, comp)
  comp = comp or f_min
  n = n or 1
  local values, count = {}, 0
  for k, v in M.sortedv(array, comp) do
    if count < n then
      count = count + 1
      values[count] = v
    end
  end
  return values
end

--- Returns a shuffled copy of a given array. If a seed is provided, it will
-- be used to init the built-in pseudo random number generator (using `math.randomseed`).
-- @name shuffle
-- @param array an array
-- @param[opt] seed a seed
-- @return a shuffled copy of the given array
function M.shuffle(array, seed)
  if seed then randomseed(seed) end
  local _shuffled = {}
  for index, value in ipairs(array) do
    local randPos = floor(random()*index)+1
    _shuffled[index] = _shuffled[randPos]
    _shuffled[randPos] = value
  end
  return _shuffled
end

--- Converts a list of arguments to an array.
-- @name pack
-- @param ... a list of arguments
-- @return an array of all passed-in args
function M.pack(...) return {...} end

--- Looks for the first occurrence of a given value in an array. Returns the value index if found.
-- Uses @{isEqual} to compare values.
-- @name find
-- @param array an array of values
-- @param value a value to lookup for
-- @param[opt] from the index from where the search will start. Defaults to 1.
-- @return the index of the value if found in the array, `nil` otherwise.
-- @see detect
function M.find(array, value, from)
  for i = from or 1, #array do
    if M.isEqual(array[i], value) then return i end
  end
end

--- Returns an array where values are in reverse order. The passed-in array should not be sparse.
-- @name reverse
-- @param array an array
-- @return a reversed array
function M.reverse(array)
  local _array = {}
  for i = #array,1,-1 do
    _array[#_array+1] = array[i]
  end
  return _array
end

--- Replaces elements in a given array with a given value. In case `i` and `j` are given
-- it will only replaces values at indexes between `[i,j]`. In case `j` is greater than the array
-- size, it will append new values, increasing the array size.
-- @name fill
-- @param array an array
-- @param value a value
-- @param[opt] i the index from which to start replacing values. Defaults to 1.
-- @param[optchain] j the index where to stop replacing values. Defaults to the array size.
-- @return the original array with values changed
function M.fill(array, value, i, j)
	j = j or M.size(array)
	for i = i or 1, j do array[i] = value end
	return array
end

--- Returns an array of `n` zeros.
-- @name zeros
-- @param n a number
-- @return an array
-- @see ones
-- @see vector
function M.zeros(n) return M.fill({}, 0, 1, n) end

--- Returns an array of `n` 1's.
-- @name ones
-- @param n a number
-- @return an array
-- @see zeros
-- @see vector
function M.ones(n) return M.fill({}, 1, 1, n) end

--- Returns an array of `n` times a given value.
-- @name vector
-- @param value a value
-- @param n a number
-- @return an array
-- @see zeros
-- @see ones
function M.vector(value, n) return M.fill({}, value, 1, n) end

--- Collects values from a given array. The passed-in array should not be sparse.
-- This function collects values as long as they satisfy a given predicate and returns on the first falsy test.
-- <br/><em>Aliased as `takeWhile`</em>
-- @name selectWhile
-- @param array an array
-- @param f an iterator function prototyped as `f (v, k)`
-- @return a new table containing all values collected
-- @see dropWhile
function M.selectWhile(array, f)
  local t = {}
  for i,v in ipairs(array) do
    if f(v,i) then t[i] = v else break end
  end
  return t
end

--- Collects values from a given array. The passed-in array should not be sparse.
-- This function collects values as long as they do not satisfy a given predicate and returns on the first truthy test.
-- <br/><em>Aliased as `rejectWhile`</em>
-- @name dropWhile
-- @param array an array
-- @param f an iterator function prototyped as `f (v, k)`
-- @return a new table containing all values collected
-- @see selectWhile
function M.dropWhile(array, f)
  local _i
  for i,v in ipairs(array) do
    if not f(v, i) then
      _i = i
      break
    end
  end
  if (_i == nil) then return {} end
  return M.rest(array,_i)
end

--- Returns the index at which a value should be inserted. This index is evaluated so 
-- that it maintains the sort. If a comparison function is passed, it will be used to sort
-- values.
-- @name sortedIndex
-- @param array an array
-- @param the value to be inserted
-- @param[opt] comp an comparison function prototyped as `f (a, b)`, defaults to <tt><</tt> operator.
-- @param[optchain] sort whether or not the passed-in array should be sorted
-- @return number the index at which the passed-in value should be inserted
function M.sortedIndex(array, value, comp, sort)
  local _comp = comp or f_min
  if (sort == true) then t_sort(array,_comp) end
  for i = 1,#array do
    if not _comp(array[i],value) then return i end
  end
  return #array+1
end

--- Returns the index of the first occurrence of value in an array.
-- @name indexOf
-- @param array an array
-- @param value the value to search for
-- @return the index of the passed-in value
-- @see lastIndexOf
function M.indexOf(array, value)
  for k = 1,#array do
    if array[k] == value then return k end
  end
end

--- Returns the index of the last occurrence of value in an array.
-- @name lastIndexOf
-- @param array an array
-- @param value the value to search for
-- @return the index of the last occurrence of the passed-in value or __nil__
-- @see indexOf
function M.lastIndexOf(array, value)
  local key = M.indexOf(M.reverse(array),value)
  if key then return #array-key+1 end
end

--- Returns the first index at which a predicate returns true.
-- @name findIndex
-- @param array an array
-- @param pred a predicate function prototyped as `pred (v, k)`
-- @return the index found or __nil__
-- @see findLastIndex
function M.findIndex(array, pred)
	for k = 1, #array do
		if pred(array[k],k) then return k end
	end
end

--- Returns the last index at which a predicate returns true.
-- @name findLastIndex
-- @param array an array
-- @param pred a predicate function prototyped as `pred (k, v)`
-- @return the index found or __nil__
-- @see findIndex
function M.findLastIndex(array, pred)
  local key = M.findIndex(M.reverse(array),pred)
  if key then return #array-key+1 end
end

--- Adds all passed-in values at the top of an array. The last elements will bubble to the
-- top of the given array.
-- @name addTop
-- @param array an array
-- @param ... a variable number of arguments
-- @return the passed-in array with new values added
-- @see prepend
-- @see push
function M.addTop(array, ...)
  for k,v in ipairs({...}) do
    t_insert(array,1,v)
  end
  return array
end

--- Adds all passed-in values at the top of an array. As opposed to @{addTop}, it preserves the order
-- of the passed-in elements.
-- @name prepend
-- @param array an array
-- @param ... a variable number of arguments
-- @return the passed-in array with new values added
-- @see addTop
-- @see push
function M.prepend(array, ...)
  return M.append({...}, array)
end

--- Pushes all passed-in values at the end of an array.
-- @name push
-- @param array an array
-- @param ... a variable number of arguments
-- @return the passed-in array with new added values
-- @see addTop
-- @see prepend
function M.push(array, ...)
  local args = {...}
  for k,v in ipairs({...}) do
    array[#array+1] = v
  end
  return array
end

--- Removes and returns the values at the top of a given array.
-- <br/><em>Aliased as `pop`</em>
-- @name shift
-- @param array an array
-- @param[opt] n the number of values to be popped. Defaults to 1.
-- @return the popped values
-- @see unshift
function M.shift(array, n)
  n = min(n or 1, #array)
  local ret = {}
  for i = 1, n do 
    local retValue = array[1]
    ret[#ret + 1] = retValue
    t_remove(array,1)
  end
  return unpack(ret)
end

--- Removes and returns the values at the end of a given array.
-- @name unshift
-- @param array an array
-- @param[opt] n the number of values to be unshifted. Defaults to 1.
-- @return the values
-- @see shift
function M.unshift(array, n)
  n = min(n or 1, #array)
  local ret = {}
  for i = 1, n do
    local retValue = array[#array]
    ret[#ret + 1] = retValue
    t_remove(array)
  end
  return unpack(ret)
end

--- Removes all provided values in a given array.
-- <br/><em>Aliased as `remove`</em>
-- @name pull
-- @param array an array
-- @param ... a variable number of values to be removed from the array
-- @return the passed-in array with values removed
function M.pull(array, ...)
  local values = {...}
  for i = #array, 1, -1 do
    local remval = false
    for k, rmValue in ipairs(values) do
      if (remval == false) then
        if M.isEqual(array[i], rmValue) then
          t_remove(array, i)
          remval = true
        end
      end
    end
  end
  return array
end

--- Removes values at an index within the range `[start, finish]`.
-- <br/><em>Aliased as `rmRange`, `chop`</em>
-- @name removeRange
-- @param array an array
-- @param[opt] start the lower bound index, defaults to the first index in the array.
-- @param[optchain] finish the upper bound index, defaults to the array length.
-- @return the passed-in array with values removed
function M.removeRange(array, start, finish)
  start = start or 1
  finish = finish or #array
  if start > finish then 
    error("start cannot be greater than finish.")
  end  
  for i = finish, start, -1 do
    t_remove(array, i)
  end
  return array
end

--- Chunks together consecutive values. Values are chunked on the basis of the return
-- value of a provided predicate `f (v, k)`. Consecutive elements which return 
-- the same value are chunked together. Leaves the first argument untouched if it is not an array.
-- @name chunk
-- @param array an array
-- @param f an iterator function prototyped as `f (v, k)`. Defaults to @{identity}.
-- @return a table of chunks (arrays)
-- @see zip
function M.chunk(array, f)
  local ch, ck, prev, val = {}, 0
  f = f or M.identity
  for k,v in ipairs(array) do
    val = f(v, k)
    ck = ((val~=prev) and (ck+1) or ck)
    prev = (prev==nil) and val or prev
    if not ch[ck] then
      ch[ck] = {array[k]}
    else
      ch[ck][#ch[ck]+1] = array[k]
    end
    prev = val
  end
  return ch
end

--- Slices values indexed within `[start, finish]` range.
-- <br/><em>Aliased as `M.sub`</em>
-- @name slice
-- @param array an array
-- @param[opt] start the lower bound index, defaults to the first index in the array.
-- @param[optchain] finish the upper bound index, defaults to the array length.
-- @return a new array of sliced values
function M.slice(array, start, finish)
  local t = {}
  for k = start or 1, finish or #array do
    t[#t+1] = array[k]
  end
  return t
end

--- Returns the first N values in an array.
-- <br/><em>Aliased as `head`, `take` </em>
-- @name first
-- @param array an array
-- @param[opt] n the number of values to be collected, defaults to 1.
-- @return a new array
-- @see initial
-- @see last
-- @see rest
function M.first(array, n)
  n = n or 1
  local t = {}
  for k = 1, n do
    t[k] = array[k]
  end
  return t
end

--- Returns all values in an array excluding the last N values.
-- @name initial
-- @param array an array
-- @param[opt] n the number of values to be left, defaults to the array length.
-- @return a new array
-- @see first
-- @see last
-- @see rest
function M.initial(array, n)
  local l = #array
  n = n and l-(min(n,l)) or l-1
  local t = {}
  for k = 1, n do
    t[k] = array[k]
  end
  return t
end

--- Returns the last N values in an array.
-- @name last
-- @param array an array
-- @param[opt] n the number of values to be collected, defaults to the array length.
-- @return a new array
-- @see first
-- @see initial
-- @see rest
function M.last(array, n)
  local l = #array
  n = n and l-min(n-1,l-1) or 2 
  local t = {}
  for k = n, l do
    t[#t+1] = array[k]
  end
  return t
end

--- Returns all values after index.
-- <br/><em>Aliased as `tail`</em>
-- @name rest
-- @param array an array
-- @param[opt] index an index, defaults to 1
-- @return a new array
-- @see first
-- @see initial
-- @see last
function M.rest(array, index)
  local t = {}
  for k = index or 1, #array do
    t[#t+1] = array[k]
  end
  return t
end

--- Returns the value at a given index.
-- @name nth
-- @param array an array
-- @param index an index
-- @return the value at the given index
function M.nth(array, index)
  return array[index]
end

--- Returns all truthy values (removes `falses` and `nils`).
-- @name compact
-- @param array an array
-- @return a new array
function M.compact(array)
  local t = {}
  for k,v in pairs(array) do
    if v then t[#t+1] = v end
  end
  return t
end

--- Flattens a nested array. Passing `shallow` will only flatten at the first level.
-- @name flatten
-- @param array an array
-- @param[opt] shallow specifies the flattening depth. Defaults to `false`.`
-- @return a flattened array
function M.flatten(array, shallow)
  shallow = shallow or false
  local new_flattened
  local _flat = {}
  for key,value in ipairs(array) do
    if type(value) == 'table' then
      new_flattened = shallow and value or M.flatten (value)
      for k,item in ipairs(new_flattened) do _flat[#_flat+1] = item end
    else _flat[#_flat+1] = value
    end
  end
  return _flat
end

--- Returns values from an array not present in all passed-in args.
-- <br/><em>Aliased as `without` and `diff`</em>
-- @name difference
-- @param array an array
-- @param another array
-- @return a new array
-- @see union
-- @see intersection
-- @see symmetricDifference
function M.difference(array, array2)
  if not array2 then return M.clone(array) end
  return M.select(array,function(value)
    return not M.include(array2,value)
  end)
end

--- Returns the duplicate-free union of all passed in arrays.
-- @name union
-- @param ... a variable number of arrays arguments
-- @return a new array
-- @see difference
-- @see intersection
-- @see symmetricDifference
function M.union(...)
  return M.unique(M.flatten({...}))
end

--- Returns the  intersection of all passed-in arrays.
-- Each value in the result is present in each of the passed-in arrays.
-- @name intersection
-- @param ... a variable number of array arguments
-- @return a new array
-- @see difference
-- @see union
-- @see symmetricDifference
function M.intersection(...)
  local arg = {...}
  local array = arg[1]
  t_remove(arg, 1)
  local _intersect = {}
  for i,value in ipairs(array) do
    if M.all(arg,function(v) return M.include(v,value) end) then
      _intersect[#_intersect+1] = value
    end
  end
  return _intersect
end

--- Checks if all passed in arrays are disjunct.
-- @name disjoint
-- @param ... a variable number of arrays
-- @return `true` if the intersection of all arrays is not empty, `false` otherwise.
-- @see intersection
function M.disjoint(...)
  return (#M.intersection(...) == 0)
end

--- Performs a symmetric difference. Returns values from `array` not present in `array2` and also values
-- from `array2` not present in `array`.
-- <br/><em>Aliased as `symdiff`</em>
-- @name symmetricDifference
-- @param array an array
-- @param array2 another array
-- @return a new array
-- @see difference
-- @see union
-- @see intersection
function M.symmetricDifference(array, array2)
  return M.difference(
    M.union(array, array2),
    M.intersection(array,array2)
  )
end

--- Produces a duplicate-free version of a given array.
-- <br/><em>Aliased as `uniq`</em>
-- @name unique
-- @param array an array
-- @return a new array, duplicate-free
-- @see isunique
-- @see duplicates
function M.unique(array)
  local ret = {}
  for i = 1, #array do
    if not M.find(ret, array[i]) then
      ret[#ret+1] = array[i]
    end
  end
  return ret
end

--- Checks if a given array contains distinct values. Such an array is made of distinct elements,
-- which only occur once in this array.
-- <br/><em>Aliased as `isuniq`</em>
-- @name isunique
-- @param array an array
-- @return `true` if the given array is unique, `false` otherwise.
-- @see unique
-- @see duplicates
function M.isunique(array)
  return #array == #(M.unique(array))
end

--- Returns an array list of all duplicates in array.
-- @name duplicates
-- @param array an array
-- @return an array-list of duplicates
-- @see unique
function M.duplicates(array)
  local dict = M.invert(array)
  local dups = {}
  for k, v in ipairs(array) do
    if dict[v] ~= k and not M.find(dups, v) then
      dups[#dups+1] = v
    end
  end
  return dups
end

--- Merges values of each of the passed-in arrays in subsets.
-- Only values indexed with the same key in the given arrays are merged in the same subset.
-- <br/><em>Aliased as `transpose`</em>
-- @name zip
-- @param ... a variable number of array arguments
-- @return a new array
-- @see zipWith
function M.zip(...)
  local args = {...}
  local n = M.max(args, function(array) return #array end)
  local _ans = {}
  for i = 1,n do
    if not _ans[i] then _ans[i] = {} end    
    for k, array in ipairs(args) do
      if (array[i]~= nil) then _ans[i][#_ans[i]+1] = array[i] end
    end
  end
  return _ans
end

--- Merges values using a given function.
-- Only values indexed with the same key in the given arrays are merged in the same subset.
-- Function `f` is used to combine values.
-- <br/><em>Aliased as `transposeWith`</em>
-- @name zipWith
-- @param f a function
-- @param ... a variable number of array arguments
-- @return a flat array of results
-- @see zip
function M.zipWith(f, ...)
  local args = {...}
  local n = M.max(args, function(array) return #array end)
  local _ans = {}
  for i = 1,n do    
    _ans[i] = f(unpack(M.pluck(args,i)))
  end
  return _ans
end

--- Clones array and appends values from another array.
-- @name append
-- @param array an array
-- @param other an array
-- @return a new array
function M.append(array, other)
  local t = {}
  for i,v in ipairs(array) do t[i] = v end
  for i,v in ipairs(other) do t[#t+1] = v end
  return t
end

--- Interleaves arrays. It returns a single array made of values from all
-- passed in arrays in their given order, interleaved.
-- @name interleave
-- @param ... a variable list of arrays
-- @return a new array
-- @see interpose
function M.interleave(...) 
  local args = {...}
  local n = M.max(args, M.size)
  local t = {}
  for i = 1, n do  
    for k, array in ipairs(args) do
      if array[i] then t[#t+1] = array[i] end
    end
  end
  return t
end

--- Interposes value in-between consecutive pair of values in array.
-- <br/><em>Aliased as `intersperse`</em>
-- @name interpose
-- @param array an array
-- @param value a value
-- @return a new array
-- @see interleave
function M.interpose(array, value)
  for k = #array, 2,-1 do
    t_insert(array, k, value)
  end
  return array
end

--- Produces a flexible list of numbers. If one value is passed, will count from 1 to that value,
-- with a default step of 1 (or -1). If two values are passed, will count from the first one to the second one,
-- using a default step of 1 (or -1). A third value passed will be considered a step value.
-- @name range
-- @param[opt] from the initial value of the range
-- @param[optchain] to the final value of the range
-- @param[optchain] step the step of count. Defaults to 1 or -1.
-- @return a new array of numbers
function M.range(from, to, step)
  if (from == nil) and (to == nil) and (step ==nil) then
    return {}
  elseif (from ~= nil) and (to == nil) and (step == nil) then
    from, to, step = signum(from), from, signum(from)
  elseif (from ~= nil) and (to ~= nil) and (step == nil) then
    step = signum(to - from)
  end
  local _ranged = {from}
  local steps = max(floor((to-from)/step),0)
  for i=1,steps do _ranged[#_ranged+1] = from+step*i end
  return _ranged
end

--- Creates an array list of `n` values, repeated.
-- @name rep
-- @param value a value to be repeated
-- @param n the number of repetitions of value.
-- @return a new array of `n` values
function M.rep(value, n)
  local ret = {}
  for i = 1, n do ret[i] = value end
  return ret
end

--- Returns the powerset of array values. For instance, when given the set {1,2,3},
-- returns `{{},{1},{2},{3},{1,2},{2,3},{1,3},{1,2,3}}`.
-- @name powerset
-- @param array an array
-- @return an array
function M.powerset(array)
  local n = #array
  local powerset = {}
  for i, v in ipairs(array) do
    for j = 1, #powerset do
      local set = powerset[j]
      t_insert(powerset, M.push(M.slice(set), v))
    end
    t_insert(powerset, {v})
  end
  t_insert(powerset, {})
  return powerset
end

--- Iterator returning partitions of an array. It returns arrays of length `n` 
-- made of values from the given array. If the last partition has lower elements than `n` and 
-- `pad` is supplied, it will be adjusted to `n` of elements with `pad` value.
-- @name partition
-- @param array an array
-- @param[opt] n the size of partitions. Defaults to 1.
-- @param[optchain] pads a value to adjust the last subsequence to the `n` elements
-- @return an iterator function
-- @see overlapping
-- @see aperture
function M.partition(array, n, pad)
	if n<=0 then return end
  return wrap(function()
    partgen(array, n or 1, yield, pad)
  end)
end

--- Iterator returning overlapping partitions of an array. 
-- If the last subsequence has lower elements than `n` and `pad` is 
-- supplied, it will be adjusted to `n` elements with `pad` value.
-- @name overlapping
-- @param array an array
-- @param[opt] n the size of partitions. Defaults to 2.
-- @param[optchain] pads a value to adjust the last subsequence to the `n` elements
-- @return an iterator function
-- @see partition
-- @see aperture
function M.overlapping(array, n, pad)
	if n<=1 then return end
  return wrap(function()
    partgen2(array, n or 2, yield, pad)
  end)
end

--- Iterator returning sliding partitions of an array.
-- <br/><em>Aliased as `sliding`</em>
-- @name aperture
-- @param array an array
-- @param[opt] n the size of partitions. Defaults to 2 (and then behaves like @{pairwise})
-- @return an iterator function
-- @see partition
-- @see overlapping
-- @see pairwise
function M.aperture(array, n)
	if n<=1 then return end
  return wrap(function()
    partgen3(array, n or 2, yield)
  end)
end

--- Iterator returning sliding pairs of an array.
-- @name pairwise
-- @param array an array
-- @return an iterator function
-- @see overlapping
function M.pairwise(array) return M.aperture(array, 2) end

--- Iterator returning the permutations of an array. It returns arrays made of all values
-- from the passed-in array, with values permuted.
-- @name permutation
-- @param array an array
-- @return an iterator function
function M.permutation(array)
  return wrap(function() 
    permgen(array, #array, yield)
  end)
end

--- Concatenates values in a given array. Handles booleans as well. If `sep` string is
-- passed, it will be used as a separator. Passing `i` and `j` will result in concatenating
-- only values within `[i, j]` range.
-- <br/><em>Aliased as `join`</em>
-- @name concat
-- @param array a given array
-- @param[opt] sep a separator string, defaults to the empty string `''`.
-- @param[optchain] i the starting index, defaults to 1.
-- @param[optchain] j the final index, defaults to the array length.
-- @return a string
function M.concat(array, sep, i, j)
  return t_concat(M.map(array,tostring),sep,i,j)
end

--- Returns all possible pairs built from given arrays.
-- @name xprod
-- @param array a first array
-- @param array2 a second array
-- @return an array list of all pairs
function M.xprod(array, array2)
  local p = {}
  for i, v1 in ipairs(array) do
    for j, v2 in ipairs(array2) do
      p[#p+1] = {v1, v2}
    end
  end
  return p
end

--- Creates pairs from value and array. Value is always prepended to the pair.
-- @name xpairs
-- @param valua a value
-- @param array an array
-- @return an array list of all pairs
function M.xpairs(value, array)
  local xpairs = {}
  for k, v in ipairs(array) do
    xpairs[k] = {value, v}
  end
  return xpairs
end

--- Creates pairs from value and array. Value is always appended as the last item to the pair.
-- @name xpairsRight
-- @param valua a value
-- @param array an array
-- @return an array list of all pairs
function M.xpairsRight(value, array)
  local xpairs = {}
  for k, v in ipairs(array) do
    xpairs[k] = {v, value}
  end
  return xpairs
end

--- Returns the sum of array values.
-- @name sum
-- @param array a given array
-- @return the sum of array values
function M.sum(array)
  local s = 0
  for k, v in ipairs(array) do s = s + v end
  return s
end

--- Returns the product of array values.
-- @name product
-- @param array a given array
-- @return the product of array values
function M.product(array)
  local p = 1
  for k, v in ipairs(array) do p = p * v end
  return p
end

--- Returns the mean of an array of numbers.
-- <br/><em>Aliased as `average`</em>
-- @name mean
-- @param array an array of numbers
-- @return a number
-- @see sum
-- @see product
-- @see median
function M.mean(array)
  return M.sum(array)/(#array)
end

--- Returns the median of an array of numbers.
-- @name median
-- @param array an array of numbers
-- @return a number
-- @see sum
-- @see product
-- @see mean
function M.median(array)
  local t = M.sort(M.clone(array))
  local n = #t
  if n == 0 then 
    return 
  elseif n==1 then 
    return t[1]
  end
  local mid = ceil(n/2)
  return n%2==0 and (t[mid] + t[mid+1])/2 or t[mid]
end

--- Utility functions
-- @section Utility functions

--- The no operation function.
-- @name noop
-- @return nothing
function M.noop() return end

--- Returns the passed-in value. This function is used internally
-- as a default iterator.
-- @name identity
-- @param value a value
-- @return the passed-in value
function M.identity(value) return value end

--- Calls `f` with the supplied arguments. Returns the results of `f(...)`.
-- @name call
-- @param f a function
-- @param[opt] ... a vararg list of args to `f`
-- @return the result of `f(...)` call.
function M.call(f, ...)
  return f(...)
end

--- Creates a constant function which returns the same output on every call.
-- <br/><em>Aliased as `always`</em>
-- @name constant
-- @param value a constant value
-- @return a constant function
function M.constant(value) 
  return function() return value end 
end

--- Returns a function which applies `specs` on args. This function produces an object having
-- the same structure than `specs` by mapping each property to the result of calling its 
-- associated function with the supplied arguments
-- @name applySpec
-- @param specs a table
-- @return a function
function M.applySpec(specs)
  return function (...)
    local spec = {}
    for i, f in pairs(specs) do spec[i] = f(...) end
    return spec
  end
end

--- Threads `value` through a series of functions. If a function expects more than one args,
-- it can be specified using an array list, where the first item is the function and the following
-- are the remaining args neeeded. The value is used as the first input.
-- @name thread
-- @param value a value
-- @param ... a vararg list of functions or arrays
-- @return a value
-- @see threadRight
function M.thread(value, ...)
  local state = value
  local arg = {...}
  for k, t in ipairs(arg) do
    if type(t) == 'function' then
      state = t(state)
    elseif type(t) == 'table' then
      local f = t[1]
      t_remove(t, 1)
      state = M.reduce(t, f, state)
    end
  end
  return state
end

--- Threads `value` through a series of functions. If a function expects more than one args,
-- it can be specified using an array list, where the first item is the function and the following
-- are the remaining args neeeded. The value is used as the last input.
-- @name threadRight
-- @param value a value
-- @param ... a vararg list of functions or arrays
-- @return a value
-- @see thread
function M.threadRight(value, ...)
  local state = value
  local arg = {...}
  for k, t in ipairs(arg) do
    if type(t) == 'function' then
      state = t(state)
    elseif type(t) == 'table' then
      local f = t[1]
      t_remove(t, 1)
      t_insert(t, state)
      state = M.reduce(t, f)
    end
  end
  return state
end

--- Returns a dispatching function. When called with arguments, this function invokes each of its functions
-- in the passed-in order and returns the results of the first non-nil evaluation.
-- @name dispatch
-- @param ... a vararg list of functions
-- @return a dispatch function
function M.dispatch(...)
  local funcs = {...}
  return function (...)
    for k, f in ipairs(funcs) do
      local r = {f(...)}
      if #r > 0 then return unpack(r) end
    end
  end
end

--- Memoizes a given function by caching the computed result.
-- Useful for speeding-up slow-running functions.
-- <br/><em>Aliased as `cache`</em>
-- @name memoize
-- @param f a function
-- @return a new function
function M.memoize(f)
  local _cache = setmetatable({},{__mode = 'kv'})
  return function (key)
      if (_cache[key] == nil) then
        _cache[key] = f(key)
      end
      return _cache[key]
    end
end

--- Builds a list from a seed value. Accepts an iterator function, which 
-- returns either nil to stop iteration or two values : the value to add to the list
-- of results and the seed to be used in the next call to the iterator function.
-- @name unfold
-- @param f an iterator function
-- @param seed a seed value
-- @return an array of values
function M.unfold(f, seed)
  local t, result = {}
  while true do
    result, seed = f(seed)
    if result ~= nil then t[#t+1] = result
    else break
    end
  end 
  return t
end

--- Returns a version of `f` that runs only once. Successive calls to `f`
-- will keep yielding the same output, no matter what the passed-in arguments are. 
-- It can be used to initialize variables.
-- @name once
-- @param f a function
-- @return a new function
-- @see before
-- @see after
function M.once(f)
  local _internal = 0
  local _args = {}
  return function(...)
		_internal = _internal+1
		if _internal <= 1 then _args = {...} end
		return f(unpack(_args))
  end
end

--- Returns a version of `f` that will run no more than <em>count</em> times. Next calls will
-- keep yielding the results of the count-th call.
-- @name before
-- @param f a function
-- @param count a count
-- @return a new function
-- @see once
-- @see after
function M.before(f, count)
  local _internal = 0
  local _args = {}
  return function(...)
		_internal = _internal+1
		if _internal <= count then _args = {...} end
		return f(unpack(_args))
  end
end

--- Returns a version of `f` that runs on the `count-th` call.
-- Useful when dealing with asynchronous tasks.
-- @name after
-- @param f a function
-- @param count the number of calls before `f` will start running.
-- @return a new function
-- @see once
-- @see before
function M.after(f, count)
  local _limit,_internal = count, 0
  return function(...)
		_internal = _internal+1
		if _internal >= _limit then return f(...) end
  end
end

--- Composes functions. Each passed-in function consumes the return value of the function that follows.
-- In math terms, composing the functions `f`, `g`, and `h` produces the function `f(g(h(...)))`.
-- @name compose
-- @param ... a variable number of functions
-- @return a new function
-- @see pipe
function M.compose(...)
	-- See: https://github.com/Yonaba/Moses/pull/15#issuecomment-139038895
  local f = M.reverse {...}
  return function (...)
		local first, _temp = true
		for i, func in ipairs(f) do
			if first then
				first = false
				_temp = func(...)
			else
				_temp = func(_temp)
			end
		end
		return _temp
	end
end

--- Pipes a value through a series of functions. In math terms, 
-- given some functions `f`, `g`, and `h` in that order, it returns `f(g(h(value)))`.
-- @name pipe
-- @param value a value
-- @param ... a variable number of functions
-- @return the result of the composition of function calls.
-- @see compose
function M.pipe(value, ...)
  return M.compose(...)(value)
end

--- Returns the logical complement of a given function. For a given input, the returned 
-- function will output `false` if the original function would have returned `true`, 
-- and vice-versa.
-- @name complement
-- @param f a function
-- @return  the logical complement of the given function `f`.
function M.complement(f)
  return function(...) return not f(...) end
end

--- Calls a sequence of passed-in functions with the same argument.
-- Returns a sequence of results. 
-- <br/><em>Aliased as `juxt`</em>
-- @name juxtapose
-- @param value a value
-- @param ... a variable number of functions
-- @return a list of results
function M.juxtapose(value, ...)
  local res = {}
  for i, func in ipairs({...}) do
    res[i] = func(value) 
  end
  return unpack(res)
end

--- Wraps `f` inside of the `wrapper` function. It passes `f` as the first argument to `wrapper`.
-- This allows the wrapper to execute code before and after `f` runs,
-- adjust the arguments, and execute it conditionally.
-- @name wrap
-- @param f a function to be wrapped, prototyped as `f (...)`
-- @param wrapper a wrapper function, prototyped as `wrapper (f, ...)`
-- @return the results
function M.wrap(f, wrapper)
  return function (...) return  wrapper(f,...) end
end

--- Runs `iter` function `n` times. Collects the results of each run and returns them in an array.
-- @name times
-- @param  iter an iterator function, prototyped as `iter (i)`
-- @param[opt] n the number of times `iter` should be called. Defaults to 1.
-- @return table an array of results
function M.times(iter, n)
  local results = {}
  for i = 1, (n or 1) do
    results[i] = iter(i)
  end
  return results
end

--- Binds `v` to be the first argument to `f`. Calling `f (...)` will result to `f (v, ...)`.
-- @name bind
-- @param f a function
-- @param v a value
-- @return a function
-- @see bind2
-- @see bindn
-- @see bindall
function M.bind(f, v)
  return function (...)
    return f(v,...)
  end
end

--- Binds `v` to be the second argument to `f`. Calling `f (a, ...)` will result to `f (a, v, ...)`.
-- @name bind2
-- @param f a function
-- @param v a value
-- @return a function
-- @see bind
-- @see bindn
-- @see bindall
function M.bind2(f, v)
  return function (t, ...)
    return f(t, v, ...)
  end
end

--- Binds `...` to be the N-first arguments to function `f`. 
-- Calling `f (a1, a2, ..., aN)` will result to `f (..., a1, a2, ...,aN)`.
-- @name bindn
-- @param f a function
-- @param ... a variable number of arguments
-- @return a function
-- @see bind
-- @see bind2
-- @see bindall
function M.bindn(f, ...)
  local args = {...}
  return function (...)
      return f(unpack(M.append(args,{...})))
    end
end

--- Binds methods to object. As such, whenever any of these methods is invoked, it 
-- always receives the object as its first argument.
-- @name bindall
-- @param obj an abject
-- @param ... a variable number of method names
-- @return the passed-in object with all methods bound to the object itself.
-- @see bind
-- @see bind2
-- @see bindn
function M.bindall(obj, ...)
	local methodNames = {...}
	for i, methodName in ipairs(methodNames) do
		local method = obj[methodName]
		if method then obj[methodName] = M.bind(method, obj) end
	end
	return obj
end

--- Returns a function which iterate over a set of conditions. It invokes each predicate,
-- passing it given values. It returns the value of the corresponding function of the first 
-- predicate to return a non-nil value.
-- @name cond
-- @param conds an array list of predicate-function pairs
-- @return the result of invoking `f(...)` of the first predicate to return a non-nil value
function M.cond(conds)
  return function(...)
    for k, condset in ipairs(conds) do
      if condset[1](...) then 
        return condset[2](...) 
      end
    end
  end
end

--- Returns a validation function. Given a set of functions, the validation function evaluates
-- to `true` only when all its funcs returns `true`.
-- @name both
-- @param ... an array list of functions
-- @return `true` when all given funcs returns true with input, false otherwise
function M.both(...)
  local funcs = {...}
  return function (...)
    for k, f in ipairs(funcs) do
      if not f(...) then return false end
    end
    return true
  end
end

--- Returns a validation function. Given a set of functions, the validation function evaluates
-- to `true` when at least one of its funcs returns `true`.
-- @name either
-- @param ... an array list of functions
-- @return `true` when one of the given funcs returns `true` with input, `false` otherwise
function M.either(...)
  local funcs = {...}
  return function (...)
    for k, f in ipairs(funcs) do
      if f(...) then return true end
    end
    return false
  end
end

--- Returns a validation function. Given a set of functions, the validation function evaluates
-- to `true` when neither of its func return `true`.
-- @name neither
-- @param ... an array list of functions
-- @return `true` when neither of the given funcs returns `true` with input, `false` otherwise
function M.neither(...)
  local funcs = {...}
  return function (...)
    for k, f in ipairs(funcs) do
      if f(...) then return false end
    end
    return true
  end
end

--- Generates an unique ID for the current session. If given a string `template`, it
-- will use this template for output formatting. Otherwise, if `template` is a function, it
-- will evaluate `template (id)`.
-- <br/><em>Aliased as `uid`</em>.
-- @name uniqueId
-- @param[opt] template either a string or a function template to format the ID
-- @return value an ID
function M.uniqueId(template)
  unique_id_counter = unique_id_counter + 1
  if template then
    if type(template) == 'string' then
      return template:format(unique_id_counter)
    elseif type(template) == 'function' then
      return template(unique_id_counter)
    end
  end
  return unique_id_counter
end

--- Produces an iterator which repeatedly apply a function `f` onto an input. 
-- Yields `value`, then `f(value)`, then `f(f(value))`, continuously.
-- <br/><em>Aliased as `iter`</em>.
-- @name iterator
-- @param f a function 
-- @param value an initial input to `f`
-- @param[opt] n the number of times the iterator should run
-- @return an iterator function
function M.iterator(f, value, n)
  local cnt = 0
	return function()
    cnt = cnt + 1
    if n and cnt > n then return end
		value = f(value)
		return value
	end
end

--- Consumes the first `n` values of a iterator then returns it.
-- @name skip
-- @param iter an iterator function 
-- @param[opt] n a number. Defaults to 1.
-- @return the given iterator
function M.skip(iter, n)
  for i = 1, (n or 1) do
    if iter() == nil then return end
  end
  return iter
end

--- Iterates over an iterator and returns its values in an array.
-- @name tabulate
-- @param ... an iterator function (returning a generator, a state and a value)
-- @return an array of results
function M.tabulate(...)
	local r = {}
	for v in ... do r[#r+1] = v end
	return r
end

--- Returns the length of an iterator. It consumes the iterator itself.
-- @name iterlen
-- @param ... an iterator function (returning a generator, a state and a value)
-- @return the iterator length
function M.iterlen(...)
	local l = 0
  for v in ... do l = l + 1 end
  return l
end

--- Casts value as an array if it is not one.
-- @name castArray
-- @param value a value
-- @return an array containing the given value
function M.castArray(value)
  return (type(value)~='table') and {value} or value
end

--- Creates a function of `f` with arguments flipped in reverse order.
-- @name flip
-- @param f a function 
-- @return a function
function M.flip(f)
	return function(...)
		return f(unpack(M.reverse({...})))
	end
end

--- Returns a function that gets the nth argument. 
-- If n is negative, the nth argument from the end is returned.
-- @name nthArg
-- @param n a number 
-- @return a function
function M.nthArg(n)
  return function (...)
    local args = {...}
    return args[(n < 0) and (#args + n + 1) or n]
  end
end

--- Returns a function which accepts up to one arg. It ignores any additional arguments.
-- @name unary
-- @param f a function
-- @return a function
-- @see ary
function M.unary(f)
  return function (...)
    local args = {...}
    return f(args[1])
  end
end

--- Returns a function which accepts up to `n` args. It ignores any additional arguments.
-- <br/><em>Aliased as `nAry`</em>.
-- @name ary
-- @param f a function
-- @param[opt] n a number. Defaults to 1.
-- @return a function
-- @see unary
function M.ary(f, n)
  n = n or 1
  return function (...)
    local args = {...}
    local fargs = {}
    for i = 1, n do fargs[i] = args[i] end
    return f(unpack(fargs))
  end
end

--- Returns a function with an arity of 0. The new function ignores any arguments passed to it.
-- @name noarg
-- @param f a function
-- @return a new function
function M.noarg(f)
  return function ()
    return f()
  end
end

--- Returns a function which runs with arguments rearranged. Arguments are passed to the 
-- returned function in the order of supplied `indexes` at call-time.
-- @name rearg
-- @param f a function
-- @param indexes an array list of indexes
-- @return a function
function M.rearg(f, indexes)
  return function(...)
    local args = {...}
    local reargs = {}
    for i, arg in ipairs(indexes) do reargs[i] = args[arg] end
    return f(unpack(reargs))
  end
end

--- Creates a function that runs transforms on all arguments it receives.
-- @name over
-- @param ... a set of functions which will receive all arguments to the returned function
-- @return a function
-- @see overEvery
-- @see overSome
-- @see overArgs
function M.over(...)
	local transforms = {...}
	return function(...)
		local r = {}
		for i,transform in ipairs(transforms) do
			r[#r+1] = transform(...)
		end
		return r
	end
end

--- Creates a validation function. The returned function checks if *all* of the given predicates return 
-- truthy when invoked with the arguments it receives.
-- @name overEvery
-- @param ... a list of predicate functions
-- @return a new function
-- @see over
-- @see overSome
-- @see overArgs
function M.overEvery(...)
	local f = M.over(...)
	return function(...)
		return M.reduce(f(...),function(state,v) return state and v end)
	end
end

--- Creates a validation function. The return function checks if *any* of a given predicates return 
-- truthy when invoked with the arguments it receives.
-- @name overSome
-- @param ... a list of predicate functions
-- @return a new function
-- @see over
-- @see overEvery
-- @see overArgs
function M.overSome(...)
	local f = M.over(...)
	return function(...)
		return M.reduce(f(...),function(state,v) return state or v end)
	end
end

--- Creates a function that invokes `f` with its arguments transformed. 1rst arguments will be passed to 
-- the 1rst transform, 2nd arg to the 2nd transform, etc. Remaining arguments will not be transformed.
-- @name overArgs
-- @param f a function
-- @param ... a list of transforms funcs prototyped as `f (v)`
-- @return the result of running `f` with its transformed arguments
-- @see over
-- @see overEvery
-- @see overSome
function M.overArgs(f,...)
	local _argf = {...}
	return function(...)
		local _args = {...}
		for i = 1,#_argf do
			local func = _argf[i]
			if _args[i] then _args[i] = func(_args[i]) end
		end
		return f(unpack(_args))
	end
end

--- Converges two functions into one.
-- @name converge
-- @param f a function
-- @param g a function
-- @param h a function
-- @return a new version of function f 
function M.converge(f, g, h) return function(...) return f(g(...),h(...)) end end

--- Partially apply a function by filling in any number of its arguments. 
-- One may pass a string `'M'` as a placeholder in the list of arguments to specify an argument 
-- that should not be pre-filled, but left open to be supplied at call-time. 
-- @name partial
-- @param f a function
-- @param ... a list of partial arguments to `f`
-- @return a new version of function f having some of it original arguments filled
-- @see partialRight
-- @see curry
function M.partial(f,...)
	local partial_args = {...}
	return function (...)
		local n_args = {...}	
		local f_args = {}
		for k,v in ipairs(partial_args) do
			f_args[k] = (v == '_') and M.shift(n_args) or v
		end
		return f(unpack(M.append(f_args,n_args)))
	end
end

--- Similar to @{partial}, but from the right.
-- @name partialRight
-- @param f a function
-- @param ... a list of partial arguments to `f`
-- @return a new version of function f having some of it original arguments filled
-- @see partialRight
-- @see curry
function M.partialRight(f,...)
	local partial_args = {...}
	return function (...)
		local n_args = {...}	
		local f_args = {}
		for k = 1,#partial_args do
			f_args[k] = (partial_args[k] == '_') and M.shift(n_args) or partial_args[k]
		end
		return f(unpack(M.append(n_args, f_args)))
	end
end

--- Curries a function. If the given function `f` takes multiple arguments, it returns another version of 
-- `f` that takes a single argument (the first of the arguments to the original function) and returns a new 
-- function that takes the remainder of the arguments and returns the result. 
-- @name curry
-- @param f a function
-- @param[opt] n_args the number of arguments expected for `f`. Defaults to 2.
-- @return a curried version of `f`
-- @see partial
-- @see partialRight
function M.curry(f, n_args)
	n_args = n_args or 2
	local _args = {}
	local function scurry(v)
		if n_args == 1 then return f(v) end
		if v ~= nil then _args[#_args+1] = v end
		if #_args < n_args then
			return scurry
		else
			local r = {f(unpack(_args))}
			_args = {}
			return unpack(r)
		end
	end
	return scurry
end

--- Returns the execution time of `f (...)` and its returned values.
-- @name time
-- @param f a function
-- @param[opt] ... optional args to `f`
-- @return the execution time and the results of `f (...)`
function M.time(f, ...)
	local stime = clock()
	local r = {f(...)}
	return clock() - stime, unpack(r)
end

--- Object functions
-- @section Object functions

--- Returns the keys of the object properties.
-- @name keys
-- @param obj an object
-- @return an array
function M.keys(obj)
  local keys = {}
  for key in pairs(obj) do keys[#keys+1] = key end
  return keys
end

--- Returns the values of the object properties.
-- @name values
-- @param obj an object
-- @return an array of values
function M.values(obj)
  local values = {}
  for key, value in pairs(obj) do values[#values+1] = value end
  return values
end

--- Returns the value at a given path in an object. 
-- Path is given as a vararg list of keys.
-- @name path
-- @param obj an object
-- @param ... a vararg list of keys
-- @return a value or nil
function M.path(obj, ...)
  local value, path = obj, {...}
  for i, p in ipairs(path) do
    if (value[p] == nil) then return end
    value = value[p]
  end
  return value
end

--- Spreads object under property path onto provided object. 
-- It is similar to @{flattenPath}, but removes object under the property path.
-- @name spreadPath
-- @param obj an object
-- @param ... a property path given as a vararg list
-- @return the passed-in object with changes
-- @see flattenPath
function M.spreadPath(obj, ...)
  local path = {...}
  for _, p in ipairs(path) do
    if obj[p] then
      for k, v in pairs(obj[p]) do 
        obj[k] = v
        obj[p][k] = nil
      end
    end
  end
  return obj
end

--- Flattens object under property path onto provided object. 
-- It is similar to @{spreadPath}, but preserves object under the property path.
-- @name flattenPath
-- @param obj an object
-- @param ... a property path given as a vararg list
-- @return the passed-in object with changes
-- @see spreadPath
function M.flattenPath(obj, ...)
  local path = {...}
  for _, p in ipairs(path) do
    if obj[p] then
      for k, v in pairs(obj[p]) do obj[k] = v end
    end
  end
  return obj
end

--- Converts key-value pairs to an array-list of `[k, v]` pairs.
-- @name kvpairs
-- @param obj an object
-- @return an array list of key-value pairs
-- @see toObj
function M.kvpairs(obj)
	local t = {}
	for k,v in pairs(obj) do t[#t+1] = {k,v} end
	return t
end

--- Converts an array list of `[k,v]` pairs to an object. Keys are taken
-- from the 1rst column in the `[k,v]` pairs sequence, associated with values in the 2nd
-- column.
-- @name toObj
-- @param kvpairs an array-list of `[k,v]` pairs
-- @return an object
-- @see kvpairs
function M.toObj(kvpairs)
	local obj = {}
	for k, v in ipairs(kvpairs) do
		obj[v[1]] = v[2]
	end
	return obj
end

--- Swaps keys with values. Produces a new object where previous keys are now values, 
-- while previous values are now keys.
-- <br/><em>Aliased as `mirror`</em>
-- @name invert
-- @param obj a given object
-- @return a new object
function M.invert(obj)
  local _ret = {}
  for k, v in pairs(obj) do
    _ret[v] = k
  end
  return _ret
end

--- Returns a function that will return the key property of any passed-in object.
-- @name property
-- @param key a key property name
-- @return a function which should accept an object as argument
-- @see propertyOf
function M.property(key)
	return function(obj) return obj[key] end
end

--- Returns a function which will return the value of an object property. 
-- @name propertyOf
-- @param obj an object
-- @return a function which should accept a key property argument
-- @see property
function M.propertyOf(obj)
	return function(key) return obj[key] end
end

--- Converts any given value to a boolean
-- @name toBoolean
-- @param value a value. Can be of any type
-- @return `true` if value is true, `false` otherwise (false or nil).
function M.toBoolean(value)
  return not not value
end

--- Extends an object properties. It copies the properties of extra passed-in objects
-- into the destination object, and returns the destination object. The last objects
-- will override properties of the same name.
-- @name extend
-- @param destObj a destination object
-- @param ... a list of objects
-- @return the destination object extended
function M.extend(destObj, ...)
  local sources = {...}
  for k, source in ipairs(sources) do
    if type(source) == 'table' then
      for key, value in pairs(source) do destObj[key] = value end
    end
  end
  return destObj
end

--- Returns a sorted list of all methods names found in an object. If the given object
-- has a metatable implementing an `__index` field pointing to another table, will also recurse on this
-- table if `recurseMt` is provided. If `obj` is omitted, it defaults to the library functions.
-- <br/><em>Aliased as `methods`</em>.
-- @name functions
-- @param[opt] obj an object. Defaults to Moses library functions.
-- @return an array-list of methods names
function M.functions(obj, recurseMt)
  obj = obj or M
  local _methods = {}
  for key, value in pairs(obj) do
    if type(value) == 'function' then
      _methods[#_methods+1] = key
    end
  end
  if recurseMt then
    local mt = getmetatable(obj)
    if mt and mt.__index then
      local mt_methods = M.functions(mt.__index, recurseMt)
      for k, fn in ipairs(mt_methods) do
        _methods[#_methods+1] = fn
      end
    end
  end
  return _methods
end

--- Clones a given object properties. If `shallow` is passed will also clone nested array properties.
-- @name clone
-- @param obj an object
-- @param[opt] shallow whether or not nested array-properties should be cloned, defaults to false.
-- @return a copy of the passed-in object
function M.clone(obj, shallow)
  if type(obj) ~= 'table' then return obj end
  local _obj = {}
  for i,v in pairs(obj) do
    if type(v) == 'table' then
      if not shallow then
        _obj[i] = M.clone(v,shallow)
      else _obj[i] = v
      end
    else
      _obj[i] = v
    end
  end
  return _obj
end

--- Invokes interceptor with the object, and then returns object.
-- The primary purpose of this method is to "tap into" a method chain, in order to perform operations 
-- on intermediate results within the chain.
-- @name tap
-- @param obj an object
-- @param f an interceptor function, should be prototyped as `f (obj)`
-- @return the passed-in object
function M.tap(obj, f)
  f(obj)
  return obj
end

--- Checks if a given object implements a property.
-- @name has
-- @param obj an object
-- @param key a key property to be checked
-- @return `true` or `false`
function M.has(obj, key)
  return obj[key]~=nil
end

--- Returns an object copy having white-listed properties.
-- <br/><em>Aliased as `choose`</em>.
-- @name pick
-- @param obj an object
-- @param ... a variable number of string keys
-- @return the filtered object
function M.pick(obj, ...)
  local whitelist = M.flatten {...}
  local _picked = {}
  for key, property in pairs(whitelist) do
    if (obj[property])~=nil then
      _picked[property] = obj[property]
    end
  end
  return _picked
end

--- Returns an object copy without black-listed properties.
-- <br/><em>Aliased as `drop`</em>.
-- @name omit
-- @param obj an object
-- @param ... a variable number of string keys
-- @return the filtered object
function M.omit(obj, ...)
  local blacklist = M.flatten {...}
  local _picked = {}
  for key, value in pairs(obj) do
    if not M.include(blacklist,key) then
      _picked[key] = value
    end
  end
  return _picked
end

--- Applies a template to an object, preserving non-nil properties.
-- <br/><em>Aliased as `defaults`</em>.
-- @name template
-- @param obj an object
-- @param[opt] template a template object. If `nil`, leaves `obj` untouched.
-- @return the passed-in object filled
function M.template(obj, template)
  if not template then return obj end
  for i, v in pairs(template) do
    if not obj[i] then obj[i] = v end
  end
  return obj
end

--- Performs a deep comparison test between two objects. Can compare strings, functions 
-- (by reference), nil, booleans. Compares tables by reference or by values. If `useMt` 
-- is passed, the equality operator `==` will be used if one of the given objects has a 
-- metatable implementing `__eq`.
-- <br/><em>Aliased as `M.compare`, `M.matches`</em>
-- @name isEqual
-- @param objA an object
-- @param objB another object
-- @param[opt] useMt whether or not `__eq` should be used, defaults to false.
-- @return `true` or `false`
-- @see allEqual
function M.isEqual(objA, objB, useMt)
  local typeObjA = type(objA)
  local typeObjB = type(objB)

  if typeObjA~=typeObjB then return false end
  if typeObjA~='table' then return (objA==objB) end

  local mtA = getmetatable(objA)
  local mtB = getmetatable(objB)

  if useMt then
    if (mtA or mtB) and (mtA.__eq or mtB.__eq) then
      return mtA.__eq(objA, objB) or mtB.__eq(objB, objA) or (objA==objB)
    end
  end

  if M.size(objA)~=M.size(objB) then return false end
  
  local vB
  for i,vA in pairs(objA) do
    vB = objB[i]
    if vB == nil or not M.isEqual(vA, vB, useMt) then return false end
  end

  for i in pairs(objB) do
    if objA[i] == nil then return false end
  end

  return true
end

--- Invokes an object method. It passes the object itself as the first argument. if `method` is not
-- callable, will return `obj[method]`.
-- @name result
-- @param obj an object
-- @param method a string key to index in object `obj`.
-- @return the returned value of `method (obj)` call
function M.result(obj, method)
  if obj[method] then
    if M.isCallable(obj[method]) then
      return obj[method](obj)
    else return obj[method]
    end
  end
  if M.isCallable(method) then
    return method(obj)
  end
end

--- Checks if the given arg is a table.
-- @name isTable
-- @param t a value to be tested
-- @return `true` or `false`
function M.isTable(t)
  return type(t) == 'table'
end

--- Checks if the given argument is callable. Assumes `obj` is callable if
-- it is either a function or a table having a metatable implementing `__call` metamethod.
-- @name isCallable
-- @param obj an object
-- @return `true` or `false`
function M.isCallable(obj)
  return 
    ((type(obj) == 'function') or
    ((type(obj) == 'table') and getmetatable(obj) and getmetatable(obj).__call~=nil) or
    false)
end

--- Checks if the given argument is an array. Assumes `obj` is an array
-- if is a table with consecutive integer keys starting at 1.
-- @name isArray
-- @param obj an object
-- @return `true` or `false`
function M.isArray(obj)
  if not (type(obj) == 'table') then return false end
  -- Thanks @Wojak and @Enrique García Cota for suggesting this
  -- See : http://love2d.org/forums/viewtopic.php?f=3&t=77255&start=40#p163624
  local i = 0
  for k in pairs(obj) do
     i = i + 1
     if obj[i] == nil then return false end
  end
  return true
end

--- Checks if the given object is iterable with `pairs` (or `ipairs`).
-- @name isIterable
-- @param obj an object
-- @return `true` if the object can be iterated with `pairs` (or `ipairs`), `false` otherwise
function M.isIterable(obj)
  return M.toBoolean((pcall(pairs, obj)))
end

--- Extends Lua's `type` function. It returns the type of the given object and also recognises
-- file userdata
-- @name type
-- @param obj an object
-- @return the given object type
function M.type(obj)
  local tp = type(obj)
  if tp == 'userdata' then
    local mt = getmetatable(obj)
    local stdout = io and io.stdout or nil
    if stdout ~= nil and mt == getmetatable(stdout) then 
      return 'file'
    end
  end
  return tp
end

--- Checks if the given pbject is empty. If `obj` is a string, will return `true`
-- if `#obj == 0`. Otherwise, if `obj` is a table, will return whether or not this table
-- is empty. If `obj` is `nil`, it will return true.
-- @name isEmpty
-- @param[opt] obj an object
-- @return `true` or `false`
function M.isEmpty(obj)
  if (obj == nil) then return true end
  if type(obj) == 'string' then return #obj==0 end
  if type(obj) == 'table' then return next(obj)==nil end
  return true
end

--- Checks if the given argument is a string.
-- @name isString
-- @param obj an object
-- @return `true` or `false`
function M.isString(obj)
  return type(obj) == 'string'
end

--- Checks if the given argument is a function.
-- @name isFunction
-- @param obj an object
-- @return `true` or `false`
function M.isFunction(obj)
   return type(obj) == 'function'
end

--- Checks if the given argument is nil.
-- @name isNil
-- @param obj an object
-- @return `true` or `false`
function M.isNil(obj)
  return obj==nil
end

--- Checks if the given argument is a number.
-- @name isNumber
-- @param obj an object
-- @return `true` or `false`
-- @see isNaN
function M.isNumber(obj)
  return type(obj) == 'number'
end

--- Checks if the given argument is NaN (see [Not-A-Number](http://en.wikipedia.org/wiki/NaN)).
-- @name isNaN
-- @param obj an object
-- @return `true` or `false`
-- @see isNumber
function M.isNaN(obj)
  return type(obj) == 'number' and obj~=obj
end

--- Checks if the given argument is a finite number.
-- @name isFinite
-- @param obj an object
-- @return `true` or `false`
function M.isFinite(obj)
  if type(obj) ~= 'number' then return false end
  return obj > -huge and obj < huge
end

--- Checks if the given argument is a boolean.
-- @name isBoolean
-- @param obj an object
-- @return `true` or `false`
function M.isBoolean(obj)
  return type(obj) == 'boolean'
end

--- Checks if the given argument is an integer.
-- @name isInteger
-- @param obj an object
-- @return `true` or `false`
function M.isInteger(obj)
  return type(obj) == 'number' and floor(obj)==obj
end

-- Aliases

do

  -- Table functions aliases
  M.forEach       = M.each
  M.forEachi      = M.eachi
  M.update        = M.adjust
  M.alleq         = M.allEqual
  M.loop          = M.cycle
  M.collect       = M.map
  M.inject        = M.reduce
  M.foldl         = M.reduce
  M.injectr       = M.reduceRight
  M.foldr         = M.reduceRight
  M.mapr          = M.mapReduce
  M.maprr         = M.mapReduceRight
  M.any           = M.include
  M.some          = M.include
  M.contains      = M.include
  M.filter        = M.select
  M.discard       = M.reject
  M.every         = M.all
  
  -- Array functions aliases
  M.takeWhile     = M.selectWhile
  M.rejectWhile   = M.dropWhile
  M.pop           = M.shift
  M.remove        = M.pull
  M.rmRange       = M.removeRange
  M.chop          = M.removeRange
  M.sub           = M.slice
  M.head          = M.first
  M.take          = M.first
  M.tail          = M.rest
  M.without       = M.difference
  M.diff          = M.difference
  M.symdiff       = M.symmetricDifference
  M.xor           = M.symmetricDifference
  M.uniq          = M.unique
  M.isuniq        = M.isunique
	M.transpose     = M.zip
  M.part          = M.partition
  M.perm          = M.permutation
  M.transposeWith = M.zipWith
  M.intersperse   = M.interpose
  M.sliding       = M.aperture
  M.mirror        = M.invert
  M.join          = M.concat
  M.average       = M.mean
  
  -- Utility functions aliases
  M.always        = M.constant
  M.cache         = M.memoize
  M.juxt          = M.juxtapose
  M.uid           = M.uniqueId
  M.iter          = M.iterator
  M.nAry          = M.ary
  
  -- Object functions aliases
  M.methods       = M.functions
  M.choose        = M.pick
  M.drop          = M.omit
  M.defaults      = M.template
  M.compare       = M.isEqual
  M.matches       = M.isEqual

end

-- Setting chaining and building interface

do

  -- Wrapper to Moses
  local f = {}

  -- Will be returned upon requiring, indexes into the wrapper
  local Moses = {}
  Moses.__index = f

  -- Wraps a value into an instance, and returns the wrapped object
  local function new(value)
    return setmetatable({_value = value, _wrapped = true}, Moses)
  end

  setmetatable(Moses,{
    __call  = function(self,v) return new(v) end, -- Calls returns to instantiation
    __index = function(t,key,...) return f[key] end  -- Redirects to the wrapper
  })

  --- Returns a wrapped object. Calling library functions as methods on this object
  -- will continue to return wrapped objects until @{obj:value} is used. Can be aliased as `M(value)`.
  -- @class function
  -- @name chain
  -- @param value a value to be wrapped
  -- @return a wrapped object
  function Moses.chain(value)
    return new(value)
  end

  --- Extracts the value of a wrapped object. Must be called on an chained object (see @{chain}).
  -- @class function
  -- @name obj:value
  -- @return the value previously wrapped
  function Moses:value()
    return self._value
  end

  -- Register chaining methods into the wrapper
  f.chain, f.value = Moses.chain, Moses.value

  -- Register all functions into the wrapper
  for fname,fct in pairs(M) do
    if fname ~= 'operator' then -- Prevents from wrapping op functions
      f[fname] = function(v, ...)
        local wrapped = type(v) == 'table' and rawget(v,'_wrapped') or false
        if wrapped then
          local _arg = v._value
          local _rslt = fct(_arg,...)
          return new(_rslt)
        else
          return fct(v,...)
        end
      end
    end
  end
  
  -- Exports all op functions
  f.operator = M.operator
  f.op       = M.operator

  --- Imports all library functions into a context.
  -- @name import
  -- @param[opt] context a context. Defaults to `_ENV or `_G`` (current environment).
  -- @param[optchain] noConflict if supplied, will not import conflicting functions in the destination context.
  -- @return the passed-in context
  f.import = function(context, noConflict)
    context = context or _ENV or _G
    local funcs = M.functions()
    for k, fname in ipairs(funcs) do
      if rawget(context, fname)~= nil then
        if not noConflict then
          rawset(context, fname, M[fname])
        end
      else
        rawset(context, fname, M[fname])
      end
    end
    return context
  end

  -- Descriptive tags
  Moses._VERSION     = 'Moses v'.._MODULEVERSION
  Moses._URL         = 'http://github.com/Yonaba/Moses'
  Moses._LICENSE     = 'MIT <http://raw.githubusercontent.com/Yonaba/Moses/master/LICENSE>'
  Moses._DESCRIPTION = 'utility-belt library for functional programming in Lua'
  
  return Moses

end
