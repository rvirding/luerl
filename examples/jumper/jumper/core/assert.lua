-- Various assertion function for API methods argument-checking

if (...) then
	
	-- Dependancies
	local _PATH = (...):gsub('%.core.assert$','')
	local Utils = require (_PATH .. '.core.utils')
	
	-- Local references
	local lua_type = type
	local floor = math.floor
	local concat = table.concat
	local next = next
	local pairs = pairs
	local getmetatable = getmetatable
	
	-- Is I an integer ?
	local function isInteger(i)
		return lua_type(i) ==('number') and (floor(i)==i)
	end
	
	-- Override lua_type to return integers
	local function type(v)
		return isInteger(v) and 'int' or lua_type(v)
	end
	
	-- Does the given array contents match a predicate type ?
	local function arrayContentsMatch(t,...)
		local n_count = Utils.arraySize(t)
		if n_count < 1 then return false end
		local init_count = t[0] and 0 or 1
		local n_count = (t[0] and n_count-1 or n_count)
		local types = {...}
		if types then types = concat(types) end
		for i=init_count,n_count,1 do
			if not t[i] then return false end
			if types then
				if not types:match(type(t[i])) then return false end
			end
		end
		return true
	end	
	
	-- Checks if arg is a valid array map
  local function isMap(m)
		if not arrayContentsMatch(m, 'table') then return false end
		local lsize = Utils.arraySize(m[next(m)])
		for k,v in pairs(m) do
			if not arrayContentsMatch(m[k], 'string', 'int') then return false end
			if Utils.arraySize(v)~=lsize then return false end
		end
		return true
  end	
	
	-- Checks if s is a valid string map
  local function isStringMap(s)
    if lua_type(s) ~= 'string' then return false end
    local w
    for row in s:gmatch('[^\n\r]+') do
      if not row then return false end
      w = w or #row
      if w ~= #row then return false end
    end
    return true
  end

	-- Does instance derive straight from class
	local function derives(instance, class)
		return getmetatable(instance) == class
	end
	
	-- Does instance inherits from class	
	local function inherits(instance, class)
		return (getmetatable(getmetatable(instance)) == class)
	end
	
	-- Is arg a boolean
	local function isBoolean(b) 
		return (b==true or b==false)
	end
	
	-- Is arg nil ?
	local function isNil(n)
		return (n==nil)
	end
	
	local function matchType(value, types)
		return types:match(type(value))	
	end
	
	return {
		arrayContentsMatch = arrayContentsMatch,
		derives = derives,
		inherits = inherits,
		isInteger = isInteger,
		isBool = isBoolean,
		isMap = isMap,
		isStrMap = isStringMap,
		isOutOfRange = isOutOfRange,
		isNil = isNil,
		type = type,
		matchType = matchType
	}

end


