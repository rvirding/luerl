--
-- @module tamale.lua
-- @author Scott Vokes <vokes.s@gmail.com>, António, P. P. Almeida <appa@perusio.net>.
-- @date   Mon Oct 12 19:12:09 2015
--
-- @brief Pattern matching library for Lua using metatables.
--
--
--

-- Table related functions.
local getmetatable = getmetatable
local setmetatable = setmetatable
local concat = table.concat
local insert = table.insert
local sort = table.sort
-- Generic operators and functions.
local assert = assert
local ipairs = ipairs
local pairs = pairs
local pcall = pcall
local type = type
local tostring = tostring
local print = print
-- String functions.
local strmatch = string.match
local sub = string.sub
local format = string.format

-- Disable global environment.
if _G.setfenv then
  setfenv(1, {})
else -- Lua 5.2.
  _ENV = nil
end

-- @table M module table.
local M = {
  _VERSION = '1.3.0',
  DEBUG = false,
  _DESCRIPTION = 'Lua library for Pattern matching',
  _COPYRIGHT = [[
                  Copyright (c) 2010 Scott Vokes <vokes.s@gmail.com>,
                  2015 António P. P. Almeida <appa@perusio.net>

                    Permission is hereby granted, free of charge, to any person
                  obtaining a copy of this software and associated documentation
                  files (the "Software"), to deal in the Software without
                  restriction, including without limitation the rights to use,
                  copy, modify, merge, publish, distribute, sublicense, and/or sell
                  copies of the Software, and to permit persons to whom the
                  Software is furnished to do so, subject to the following
                    conditions:

                    The above copyright notice and this permission notice shall be
                    included in all copies or substantial portions of the Software.

                    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
                    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
                    OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
                    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
                    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
                    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
                    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
                    OTHER DEALINGS IN THE SOFTWARE. ]]
}

--- Debugging helper function. Prints the arguments using
--  string.format.
--
-- @local
--
-- @return nothing
--   Side effects only.
local function trace(...) print(format(...)) end

--- Mark a given table as a variable or a nil value. We need this as a
--  way to distinguish variables and empty indexes.
--
-- @param desc string description of the sentinel type.
--
-- @return table
--   table with a metatable such that the __tostring metamethod
--   returns the description, i.e., the argument.
local function sentinel(desc)
  return setmetatable({}, { __tostring = function() return desc end })
end

-- Create a marker/sentinel for variables. It allows to be used not
-- only as a key to a table, but also to quickly identify a logical
-- variable.
local VAR = sentinel('[var]')

-- Create our own nil value so that all the predicates work without
-- generating errors. NIL is used not only as a table key but also the
-- case when the indexing function is empty.
local NIL = sentinel('[nil]')

--- Predicate that tests if a given table is a variable in the pattern
--  matching/unification sense.
--
-- @local
--
-- @param t table table being tested.
--
-- @return boolean
--   true if it is, false if not.
local function is_var(t)
  -- We check to see if the metatable is such that the __tostring
  -- metamethod is the same as the VAR sentinel.
  return getmetatable(t) == VAR
end

--- Marks a string in a match pattern as a variable key.
-- (You probably want to alias this locally to something short.)
-- Any variables beginning with _ are ignored. If a variable is named
-- '...' then we capture all the array portion values.
--
-- @param name string variable name.
--
-- @return table
--   Variable definition.
function M.var(name)
  -- It must be a string.
  assert(type(name) == 'string', 'Variable name must be a string.')
  -- Check to see if we have to ignore any variables.
  local ignore = (sub(name, 1, 1) == '_')
  -- Check to see if we have a variable that has a possible infinite
  -- number of identifiers.
  local rest = (name == '...')
  -- Return a table marking it as variable through the __tostring
  -- metamethod (VAR == '[var]').
  return setmetatable( { name = name, ignore = ignore, rest = rest }, VAR)
end

--- Returns a function that tests a string with string:match, rather
--  than ==. Any captures from the string match are appended to the
--  capture table. Like var, this would probably be locally aliased,
--  and used like { P'num (%d+)', handler }.
--
-- @param str string  the Lua pattern to use in the test.
--
-- @return function
--   Closure over the given pattern to do the matching.
function M.P(str)
  -- Return a closure over the string (pattern for string matching).
  return
    function(v)
    if type(v) == 'string' then return strmatch(v, str) end
    end
end

--- Match failure default handler.
--
-- @param val mixed the unmatched value.
--
-- @return nil, string, mixed
--   nil, a notification, unmatched value.
function M.match_fail(val)
  return nil, 'Match failed', val
end

-- Key-weak cache for table counts, since #t only gives the
-- length of the array portion, and otherwise, values with extra
-- non-numeric keys can match rows that do not have them. This is a
-- memoization strategy.
local counts = setmetatable({}, { __mode = 'k' })

--- Count the number of fields in a table. This is used to make sure
--  the number of fields in the pattern and in the value are the same
--  for *non partial* matches.
--
-- @local
--
-- @param t table pattern or value to be used as cache key.
--
-- @return table
--   Field counts cache.
local function get_count(t)
  local v = counts[t]
  -- If the cache is not set, set it up.
  if not v then
    v = 0
    -- Loop over the table. Get it's length, i.e., the number of
    -- fields in the pattern or value (t).
    for  _  in pairs(t) do v = v + 1 end
    -- Set the cache value.
    counts[t] = v
  end
  -- Return the cache (weak key table).
  return v
end

--- Performs unification of an expression. The main logic for pattern
--  matching. Structurally matches a value against a pattern, setting
--  variables in the pattern to the corresponding values in val, and
--  recursively unifying table fields. Functions are treated as
--  predicates - any non-false result(s) are considered a success and
--  are captured.
--
-- @local
--
-- @param pat mixed pattern to be matched.
-- @param val mixed value being tested for matching.
-- @param cs table captures, i.e., table with unified expressions.
-- @param ids table patterns that should be matched 'strictly' (==).
-- @param row table current row of the matching matrix.
--
-- @return table or boolean
--   Unified expressions if matched, false if not.
local function unify(pat, val, cs, ids, row)
  -- Create locals for pattern and value type.
  local pat_type, val_type = type(pat), type(val)
  -- 1. Table patterns.
  if pat_type == 'table' then
    -- Unification for a variable.
    if is_var(pat) then
      -- Initialize cur to be the capture for this variable.
      local cur = cs[pat.name]
      -- If the value is already set then we cannot proceed.
      if cur and cur ~= val and not pat.ignore then return false end
      -- If is not the set it to the value: unify.
      cs[pat.name] = val
      -- Return the captures.
      return cs
    end
    -- If the value being compared is not a table bailout.
    if val_type ~= 'table' then return false end
    -- If a list of ids is set check for the matching immediately.
    if ids[pat] and pat ~= val then
      return false
    else
      -- Loop over all fields of the current pattern table.
      for k, v in pairs(pat) do
        -- If the matching fails for any of the matches then it
        -- doesn't match.
        if not unify(v, val[k], cs, ids, row) then return false end
      end
    end
    -- Make sure that the number of fields in the pattern and in the
    -- value are the same. This only makes sense if the matching is
    -- *not* partial.
    if not row.partial then
      if get_count(pat) ~= get_count(val) then return false end
      -- If we have an array of variables then we must iterate over
      -- the values to unify each variable.
    elseif row.rest then
      local rest, len = {}, #val
      -- Better perfomance with fixed length loop.
      for i = 1, len do rest[i] = val[i] end
      -- Return the captures.
      cs['...'] = rest
    end
    -- Return the captures.
    return cs
    -- 2. Function patterns.
  elseif pat_type == 'function' then
    -- If the pattern is a function , this includes patterns
    -- 'P'. Since we can have mulitple values as return insert it in a
    -- table.
    local fv = { pat(val) }
    local len_fv = #fv
    -- If the function returns nil or false the matching failed.
    if len_fv == 0 or not fv[1] then return false end
    -- Otherwise unify (capture). Insert it in the array component of
    -- the captures.
    local len_cs = #cs
    for i = 1, len_fv do cs[len_cs + i] = fv[i] end
    -- Return the captures.
    return cs
    -- 3. 'Atomic' (literal) patterns. i.e., strings, numbers.
  else
    -- If we're here then the pattern is an 'atom' and we just do a
    -- comparison with the value. If is equal return the captures
    -- If not the matching fails.
    return pat == val and cs or false
  end
end

--- Replace any occurence of variables in the result with their
--  captures. The result is the value to return for the matched
--  pattern.
--
-- @local
--
-- @param res table result, i.e., component of the pattern that got
--              matched.
-- @param u table captured variables.
--
-- @return table
--   unified expression for the result (substitutions).
local function substituted(res, u)
  -- If the component of the pattern is variable replace it by the
  -- capture and return the result.
  if is_var(res) then return u[res.name] end
  -- If is not a variable, then is a table, iterate through it and do
  -- the substitutions.
  local r = {}
  for k, v in pairs(res) do
    if type(v) == 'table' then
      if is_var(v) then
        -- If it's a variable, substitute it right away.
        r[k] = u[v.name]
        -- Call recursively the function if the value is a table and
        -- not a variable.
      else
        r[k] = substituted(v, u)
      end
    else
      -- Otherwise we matched the values directly, the result will be
      -- the values we matched.
      r[k] = v
    end
  end
  -- Return the substituted values.
  return r
end

--- Return or execute the result, substituting any vars present.
--
-- @local
--
-- @param res mixed the result expression for the matched pattern.
-- @param u table captures for variables.
-- @param has_vars boolean_vars if there are variables
--
-- @return mixed|table, table
--   * the result of invoking the function if a function.
--   * the expression with replaced vars and the captures.
--     if there are variables. (tables)
--   * the expression and the captures. (tables)
local function do_res(res, u, has_vars)
  local result_t = type(res)
  -- If it's a function invoke it and return the result.
  if result_t == 'function' then
    return res(u)
    -- If a table check
  elseif result_t == 'table' and has_vars then
    return substituted(res, u), u
  end
  -- If there are no vars return the original expression and the
  -- unified expression.
  return res, u
end

--- Predicate to determine if a result, i.e., the returned value for
--  the matched pattern has any variables.
--
-- @local
--
-- @param res table the result.
--
-- @return boolean
--   true if it has, false if not.
local function has_vars(res)
  if type(res) ~= 'table' then return false end
  if is_var(res) then return true end
  for _, v in pairs(res) do
    if type(v) == 'table' then
      if is_var(v) or has_vars(v) then return true end
    end
  end
  return false
end

--- Predicate to test if a pattern component is indexable. Only
--  tables, numbers, booleans and strings are indexable. Variables and
--  functions are not indexable. Functions are not indexable because
--  there's no universal way of computing a result that can be
--  compared.
--
-- @local
--
-- @param pat_field mixed_field a field in a pattern.
--
-- @return boolean
--   true if indexable, false if not.
local function indexable(pat_field)
  return not is_var(pat_field) and type(pat_field) ~= 'function'
end

--- Appends a value to an array. This is used for the indexing. Making
--  sure that non-indexable rows are always visited for matching, in
--  the eventuality of the index failing.
--
-- @local
--
-- @param t table to append to.
-- @param key mixed index value .
-- @param val number row id to append.
--
-- @return nothing
--   Side effects only.
local function append(t, key, val)
  local arr = t[key] or {}
  arr[#arr + 1] = val
  t[key] = arr
end

-- If the list of row IDs didn't exist when the var row was
-- indexed (and thus didn't get added), add it here.

--- Prepend the non-indexable row ids to each indexable row
--  such that we have "immutable" components in the index. This
--  makes the matching process proceed event when the indexing
--  fails. That's why need to add the non-indexable row ids to
--  the result.
--
-- @local
--
-- @param vars table variables present in the rows.
-- @param lists table non-indexable rows IDs to be prepended.
--
-- @return nothing
--   Side effects only.
local function prepend_vars(vars, lists)
  for i = #vars, 1, -1 do
    local vid = vars[i]
    for _, l in pairs(lists) do
      if l[1] > vid then insert(l, 1, vid) end
    end
  end
end

--- Since we can have "SQL like" indexes we must compute it for each
--  row of the pattern specification. We need to iterate over all
--  rows and for each compute the index. By default the index function
--  just takes the first entry of the pattern specification, i.e., for
--  a rule p we take p[1] as the index. An indexing function can be
--  specified, in this case we must compute the function for each row.
--  When there is a variable or a function in the pattern
--  specification we have to put it in a separate table since they're
--  not indexable. We need also to take care of variables in the
--  result.
--  The index values are used as keys for the tables that hold
--  the ids of the rows that are indexable. For non indexable rows we
--  prepend the row index to the array that stores the index values as
--  to make it not influence the search for the index value when
--  matching by index.
--
--  Example:
--  row 2 is indexable with index function value 'ab'. Rows 1 and 3
--  are not indexable. Then the value of the index for row 2 is:
--  ab = { 1, 3, 2 } similarly if for row 4 the index function value
--  is the index value is df = { 1, 3, 4 }.
--
-- Note that we need to return all the row ids anyway because since
-- the index searching mechanism might fail or not be relied upon for
-- the matching and we need to make sure that the matching of the
-- patterns row by row continues.
--
-- @local
--
-- @param spec table pattern specification:
--              array of { pattern,  result, [ when function ] }
--
-- @return table
--   Indexes for each row of the pattern specification.
--   Index values are the keys and the row index the values.
local function index_spec(spec)
  -- Lists of rows keyed on the index value where the pattern is a
  -- either a number, a boolean, a string or a function. Literals.
  local ls = {}
  -- Lists of rows keyed on the index value where the pattern is a
  -- table.
  local ts = {}
  -- Lists of rows keyed on the row id that are non indexable. Since
  -- they should not influence the index values as not to influence
  -- the search. They appear in both index values for literal patterns
  -- and table patterns.
  local lni, tni = {}, {}
  -- Set with the rows where there are variables in the result.
  local vrs = {}
  -- Debugging flag.
  local debug = spec.debug
  -- Field to index by: defaults to t[1].
  local ispec, indexer
  -- If the indexing is off then skip the indexing.
  if spec.index == false then
    -- If a spec has index = false, then don't index it.
    ispec = false
  else
    -- Otherwise index it, either using the supplied index function
    -- that takes a rule as argument or just use the first field of
    -- the pattern.
    ispec = spec.index or 1
  end
  -- Build the indexer function from the specification.
  if type(ispec) == 'function' then
    -- The indexer can be a user supplied function.
    indexer = ispec
  elseif ispec == 'false' then
    -- If false: no indexing at all. Empty indexing function.
    indexer = function() end
  else
    -- Otherwise use the default indexing function.
    indexer = function(t) return t[ispec] end
  end
  -- Put it in the specification table.
  spec.indexer = indexer
  -- Loop over all specification rows and compute the index for each
  -- row.
  local slen = #spec
  local row
  for i = 1, slen do
    -- Get the current row.
    row = spec[i]
    -- A rule is of the form { rule, result, [ when function ] }.
    --  Get the pattern and the result.
    local pat, res = spec[i][1], spec[i][2]
    -- Get the pattern type.
    local pat_type = type(pat)

    if not indexable(pat) then
      -- If it's non indexable then put it in the non indexable
      -- tables. It can be either a function or a variable.
      if debug then
        trace(' * rule %d: not indexable, adding to all.', i)
      end
      -- Set the current row index in the non-indexable tables. It can
      -- be a function or a table. It should not influence the index
      -- for either literal or table patterns. Add it to both since in
      -- both cases we have to make sure that the pattern matching
      -- is made row by row for *all* rows.
      lni[#lni + 1] = i
      tni[#tni + 1] = i
      -- Loop over the indexable fields (literals and tables). We
      -- append the non indexable rule index to the index so that is
      -- the index value. The value will be just the row index.
      for _, l in ipairs({ls, ts}) do
        -- Append the value to the table of index values for the
        -- indexable rows. This guarantees that in the event of
        -- index matching failure we'll be able to proceed with the
        -- 'pure' pattern matching row by row for all rows.
        for k in pairs(l) do append(l, k, i) end
      end
    elseif pat_type == 'table' then
      -- If the pattern type is a table we compute the index function
      -- value.
      local v = indexer(pat) or NIL
      -- If the value is not indexable then we cannot perform any type
      -- search for the index value for matching by index. Therefore we
      -- add it to the values of the index for each indexable row.
      if not indexable(v) then
        if debug then
          trace(' * rule %d: index(table) is not indexable.', i)
        end
        -- Append to each indexable row index value.
        for k in pairs(ts) do append(ts, k, i) end
        -- Insert the current row id in the list of non table patterns
        -- that are non-indexable. Make sure we proceed with the
        -- matching row by row when indexing fails.
        tni[#tni + 1] = i
      else
        -- Otherwise we insert the index function value in the index
        -- value (array) for the current row.
        if debug then
          trace(' * rule %d: indexing on index(t) = %s.', i,
                tostring(v))
        end
        -- Append to the list of indexable rows the index function
        -- value.
        append(ts, v, i)
      end
      -- If the pattern is a table then we have to loop over all
      -- fields to compute the index.
      local plen = #pat
      for j = 1, plen do
        -- Check for the V'...' special variable.
        if is_var(pat[j]) and pat[j].rest then
          if debug then
            trace(" * rule %d: V'...' found in field %d.", i, j)
          end
          -- Since this variable can have an infinite number of
          -- captures the matching will always be *partial*.
          row.partial = true
          -- The current row has a field that can match anything.
          row.rest = j
          break
        end
      end
    else
      if debug then
        trace(' * rule %d: indexing on %s.', i, tostring(pat))
      end
      -- Otherwise the pattern is a literal and we add the row id to
      -- the list of indexable rows with literals.
      append(ls, pat, i)
    end
    -- If there are variables in the result we add to the set of rows
    -- with variables in the result.
    if has_vars(res) then
      if debug then
        trace(' * rule %d: found var(s) in result.', i)
      end
      vrs[i] = true
    end
  end -- for
  -- For each row prepend to the index value to the array with the
  -- computed value of the index function for rows with indexable
  -- literals as pattern.
  prepend_vars(lni, ls)
  -- For each row prepend to the index value to the array with the
  -- computed value of the index function for rows with indexable
  -- tables as pattern.
  prepend_vars(tni, ts)
  -- Even if a row is non indexable we get an index value that is
  -- the array with all the row ids of the non indexable rows.
  -- If row 1 and 3 are non indexable then the index value for each is
  -- { 1 ,3 }.
  ls[VAR] = lni
  ts[VAR] = tni
  -- Return the table with the index value computed for each row.
  return { ls = ls, ts = ts, vrs = vrs }
end

--- Search for a match using the indexes, if applicable. Always
--  make sure to return all the row ids since if the indexing fails
--  i.e., we cannot find a match by index, we have to proceed for
--  pure pattern matching row by row for *all* rows.
--
-- @local
--
-- @param spec table table with the patterns to be matched.
-- @param input mixed value to match against.
-- @param idx table index specfication.
--
-- @return table
--   array with indexable row ids appended to the non indexable
--   row ids if matched. array with non-indexable row ids if
--   not matched.
local function check_index(spec, input, idx)
  local input_t = type(input)
  -- If the input value is a table we try to get the row id, that
  -- tells us which row is matched right away.
  if input_t == 'table' then
    local key = spec.indexer(input) or NIL
    local ts = idx.ts
    -- If we have a match return the array with the row id and all
    -- the non indexable row ids prepended. Otherwise return an
    -- array with all the non-indexable rows ids.
    return ts[key] or ts[VAR]
  else
    -- Otherwise the input is either a number, a bolean, a string or
    -- a function. Try to get a match.
    local ls = idx.ls
    -- If we have a match return the array with the row id and all
    -- the non indexable row ids prepended. Otherwise return an
    -- array with all the non-indexable rows ids. This is to ensure
    -- that we proceed with the matching row ny row when the index
    -- matching fails. Index matching is just a mechanism to speed
    -- up the matching.
    return ls[input] or ls[VAR]
  end
end

--- Return a matcher function for a given specification. When the
--  function is called on one or more values, its first argument is
--  tested in order against every rule that could possibly match it,
--  selecting the relevant result (if any) or returning the values
--  (false, 'Match failed', val).
--  If the result is a function, it is called with a table containing
--  any captures and any subsequent arguments passed to the matcher
--  function (in captures.args).
--
-- @param spec table A list of rows, where each row is of the form
--   { rule, result, [ when = capture_predicate ] }.
--
-- @usage spec.ids An optional list of table values that should be
--    compared by identity, not structure. If any empty tables are
--    being used as a sentinel value (e.g. 'MAGIC_ID = {}'), list
--    them here.
-- @usage spec.debug = true Turn on debugging traces for the matcher.
--
-- @return mixed
--   the result for the matching pattern or false if not.
function M.matcher(spec)
  local debug = spec.debug or M.DEBUG
  -- Set with all row ids that are to be compared using '=='.
  local ids = {}
  -- Create the set with the row ids where the comparison is to be
  -- done using equality: '=='.
  if spec.ids then
    for _, id in ipairs(spec.ids) do ids[id] = true end
  end
  -- Build the index values for all rows.
  local idx = index_spec(spec)
  -- Get the results that involve variables for later substitution by
  -- values after reaching a goal through unification.
  local vrs = idx.vrs  --variable rows
  -- Closure over the above tables that implements the matching
  -- function, i.e., the matcher itself.
  return
    function (t, ...)
    -- Search for an index matching right away. If it succeeds we just
    -- do one iteration on the for loop below, i.e., we do the
    -- unification and substitution in a single row. The one that
    -- matches the index of the input value t.
    local rows = check_index(spec, t, idx)

    if debug then
      trace(' -- Checking rules: %s.', concat(rows, ', '))
    end
    -- Loop over all rows of the specification.
    for _, id in ipairs(rows) do
      local row = spec[id]
      -- Get the pattern, result and when function, aka, matching
      -- callback.
      local pat, res, when = row[1], row[2], row.when
      -- Sanity check. No specfication makes sense without a result.
      -- A result is the return value for the matching.
      if debug and res == nil then
        trace(' -- Missing result.')
      end
      -- The matcher 'factory' can have extra arguments besides the
      -- matcher specification. Grab them.
      local args = { ... }
      -- Perform the unification for of the matching expression in the
      -- current row with the corresponding result. The third argument
      -- is to make sure that any extra arguments will be in the
      -- capture table args key. This allows for accumulators and
      -- other type of functions to be used for computing the result
      -- or/and as predicates.
      local u = unify(pat, t, { args = args }, ids, row)
      if debug then
        trace(' -- Trying rule %d...%s ', id, u and 'matched.' or 'failed.')
      end
      -- Handle the unification process output.
      if u then
        -- If the unification succeeded then set the unification input
        -- field to the result value t.
        u.input = t
        -- If we have a callback for the matching invoke it now, since
        -- the matching succeeded.
        if when then
          -- Wrap the call in pcall to avoid breaking the matching
          -- process too early. If indexing fail we have to iterate
          -- through all the rows looking for a match.
          local ok, val = pcall(when, u)
          if debug then
            trace(' -- Running when(captures) check...%s ',
              (ok and val) and 'matched.' or 'failed.')
          end
          -- Perform substitution on the variables that eventually are
          -- present in the result if the callback invocation
          -- succeeded. If nit than the matching fails. Note that the
          -- when function callback is treated as a predicate for a
          -- rule. The matching succeeds when it returns true and
          -- fails when it returns false.
          if ok and val then
            return do_res(res, u, vrs[id])
          end
        else
          -- There's no when callback just perform the substitutions
          -- of the variables eventually present in the result
          -- expresssion.
          return do_res(res, u, vrs[id])
        end -- if when
      end -- if u
    end -- for _, id in ipairs(rows)
    -- If we're here that means the matching failed.
    if debug then trace(' -- Failed.') end
    -- Invoke the failure handler in this situation.
    local fail = spec.fail or M.match_fail
    -- Return the result.
    return fail(t)
    end -- anonymous function (closure)
end -- matcher

-- Return the module table.
return setmetatable(M, { __call = function(_, ...) return M.matcher(...) end })
