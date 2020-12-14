-- Various utilities for Jumper top-level modules

if (...) then

	-- Dependencies
	local _PATH = (...):gsub('%.utils$','')
	local Path = require (_PATH .. '.path')
	local Node = require (_PATH .. '.node')

	-- Local references
	local pairs = pairs
	local type = type
	local t_insert = table.insert
	local assert = assert
	local coroutine = coroutine

	-- Raw array items count
	local function arraySize(t)
		local count = 0
		for k,v in pairs(t) do
			count = count+1
		end
		return count
	end

	-- Parses a string map and builds an array map
  local function stringMapToArray(str)
		local map = {}
		local w, h
    for line in str:gmatch('[^\n\r]+') do
      if line then
        w = not w and #line or w
        assert(#line == w, 'Error parsing map, rows must have the same size!')
        h = (h or 0) + 1
        map[h] = {}
        for char in line:gmatch('.') do
					map[h][#map[h]+1] = char
				end
      end
    end
    return map
  end

	-- Collects and returns the keys of a given array
  local function getKeys(t)
    local keys = {}
    for k,v in pairs(t) do keys[#keys+1] = k end
    return keys
  end

	-- Calculates the bounds of a 2d array
  local function getArrayBounds(map)
    local min_x, max_x
    local min_y, max_y
      for y in pairs(map) do
        min_y = not min_y and y or (y<min_y and y or min_y)
        max_y = not max_y and y or (y>max_y and y or max_y)
        for x in pairs(map[y]) do
          min_x = not min_x and x or (x<min_x and x or min_x)
          max_x = not max_x and x or (x>max_x and x or max_x)
        end
      end
    return min_x,max_x,min_y,max_y
  end

  -- Converts an array to a set of nodes
  local function arrayToNodes(map)
    local min_x, max_x
    local min_y, max_y
    local nodes = {}
      for y in pairs(map) do
        min_y = not min_y and y or (y<min_y and y or min_y)
        max_y = not max_y and y or (y>max_y and y or max_y)
        nodes[y] = {}
        for x in pairs(map[y]) do
          min_x = not min_x and x or (x<min_x and x or min_x)
          max_x = not max_x and x or (x>max_x and x or max_x)
          nodes[y][x] = Node:new(x,y)
        end
      end
    return nodes,
			 (min_x or 0), (max_x or 0),
			 (min_y or 0), (max_y or 0)
  end

	-- Iterator, wrapped within a coroutine
	-- Iterates around a given position following the outline of a square
	local function around()
		local iterf = function(x0, y0, s)
			local x, y = x0-s, y0-s
			coroutine.yield(x, y)
			repeat
				x = x + 1
				coroutine.yield(x,y)
			until x == x0+s
			repeat
				y = y + 1
				coroutine.yield(x,y)
			until y == y0 + s
			repeat
				x = x - 1
				coroutine.yield(x, y)
			until x == x0-s
			repeat
				y = y - 1
				coroutine.yield(x,y)
			until y == y0-s+1
		end
		return coroutine.create(iterf)
	end

	-- Extract a path from a given start/end position
  local function traceBackPath(finder, node, startNode)
    local path = Path:new()
    path._grid = finder._grid
    while true do
      if node._parent then
        t_insert(path._nodes,1,node)
        node = node._parent
      else
        t_insert(path._nodes,1,startNode)
        return path
      end
    end
  end

	-- Lookup for value in a table
	local indexOf = function(t,v)
		for i = 1,#t do
			if t[i] == v then return i end
		end
		return nil
	end

	-- Is i out of range
  local function outOfRange(i,low,up)
    return (i< low or i > up)
  end
	
	return {
		arraySize = arraySize,
		getKeys = getKeys,
		indexOf = indexOf,
		outOfRange = outOfRange,
		getArrayBounds = getArrayBounds,
		arrayToNodes = arrayToNodes,
		strToMap = stringMapToArray,
		around = around,
		drAround = drAround,
		traceBackPath = traceBackPath
	}

end
