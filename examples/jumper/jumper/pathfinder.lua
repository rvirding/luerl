--- The Pathfinder class

--
-- Implementation of the `pathfinder` class.

local _VERSION = ""
local _RELEASEDATE = ""

if (...) then

  -- Dependencies
  local _PATH = (...):gsub('%.pathfinder$','')
	local Utils     = require (_PATH .. '.core.utils')
	local Assert    = require (_PATH .. '.core.assert')
  local Heap      = require (_PATH .. '.core.bheap')
  local Heuristic = require (_PATH .. '.core.heuristics')
  local Grid      = require (_PATH .. '.grid')
  local Path      = require (_PATH .. '.core.path')

  -- Internalization
  local t_insert, t_remove = table.insert, table.remove
	local floor = math.floor
  local pairs = pairs
  local assert = assert
	local type = type
  local setmetatable, getmetatable = setmetatable, getmetatable

	--- Finders (search algorithms implemented). Refers to the search algorithms actually implemented in Jumper.
	--
	-- <li>[A*](http://en.wikipedia.org/wiki/A*_search_algorithm)</li>
	-- <li>[Dijkstra](http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)</li>
	-- <li>[Theta Astar](http://aigamedev.com/open/tutorials/theta-star-any-angle-paths/)</li>
	-- <li>[BFS](http://en.wikipedia.org/wiki/Breadth-first_search)</li>
	-- <li>[DFS](http://en.wikipedia.org/wiki/Depth-first_search)</li>
	-- <li>[JPS](http://harablog.wordpress.com/2011/09/07/jump-point-search/)</li>
	-- @finder Finders
	-- @see Pathfinder:getFinders
  local Finders = {
    ['ASTAR']     = require (_PATH .. '.search.astar'),
    ['DIJKSTRA']  = require (_PATH .. '.search.dijkstra'),
    ['THETASTAR'] = require (_PATH .. '.search.thetastar'),
    ['BFS']       = require (_PATH .. '.search.bfs'),
    ['DFS']       = require (_PATH .. '.search.dfs'),
    ['JPS']       = require (_PATH .. '.search.jps')
  }

  -- Will keep track of all nodes expanded during the search
  -- to easily reset their properties for the next pathfinding call
  local toClear = {}

	--- Search modes. Refers to the search modes. In ORTHOGONAL mode, 4-directions are only possible when moving,
	-- including North, East, West, South. In DIAGONAL mode, 8-directions are possible when moving,
	-- including North, East, West, South and adjacent directions.
	--
	-- <li>ORTHOGONAL</li>
	-- <li>DIAGONAL</li>
	-- @mode Modes
	-- @see Pathfinder:getModes
  local searchModes = {['DIAGONAL'] = true, ['ORTHOGONAL'] = true}

  -- Performs a traceback from the goal node to the start node
  -- Only happens when the path was found

	--- The `Pathfinder` class.<br/>
	-- This class is callable.
	-- Therefore,_ <code>Pathfinder(...)</code> _acts as a shortcut to_ <code>Pathfinder:new(...)</code>.
	-- @type Pathfinder
  local Pathfinder = {}
  Pathfinder.__index = Pathfinder

  --- Inits a new `pathfinder`
  -- @class function
  -- @tparam grid grid a `grid`
  -- @tparam[opt] string finderName the name of the `Finder` (search algorithm) to be used for search.
	-- Defaults to `ASTAR` when not given (see @{Pathfinder:getFinders}).
  -- @tparam[optchain] string|int|func walkable the value for __walkable__ nodes.
  -- If this parameter is a function, it should be prototyped as __f(value)__, returning a boolean:
  -- __true__ when value matches a __walkable__ `node`, __false__ otherwise.
  -- @treturn pathfinder a new `pathfinder` instance
	-- @usage
	-- -- Example one
	-- local finder = Pathfinder:new(myGrid, 'ASTAR', 0)
	--
	-- -- Example two
	-- local function walkable(value)
	--   return value > 0
	-- end
	-- local finder = Pathfinder(myGrid, 'JPS', walkable)
  function Pathfinder:new(grid, finderName, walkable)
    local newPathfinder = {}
    setmetatable(newPathfinder, Pathfinder)
	  newPathfinder:setGrid(grid)
    newPathfinder:setFinder(finderName)
    newPathfinder:setWalkable(walkable)
    newPathfinder:setMode('DIAGONAL')
    newPathfinder:setHeuristic('MANHATTAN')
    newPathfinder:setTunnelling(false)
    return newPathfinder
  end

	--- Evaluates [clearance](http://aigamedev.com/open/tutorial/clearance-based-pathfinding/#TheTrueClearanceMetric)
	-- for the whole `grid`. It should be called only once, unless the collision map or the
	-- __walkable__ attribute changes. The clearance values are calculated and cached within the grid nodes.
  -- @class function
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage myFinder:annotateGrid()
	function Pathfinder:annotateGrid()
		assert(self._walkable, 'Finder must implement a walkable value')
		for x=self._grid._max_x,self._grid._min_x,-1 do
			for y=self._grid._max_y,self._grid._min_y,-1 do
				local node = self._grid:getNodeAt(x,y)
				if self._grid:isWalkableAt(x,y,self._walkable) then
					local nr = self._grid:getNodeAt(node._x+1, node._y)
					local nrd = self._grid:getNodeAt(node._x+1, node._y+1)
					local nd = self._grid:getNodeAt(node._x, node._y+1)
					if nr and nrd and nd then
						local m = nrd._clearance[self._walkable] or 0
						m = (nd._clearance[self._walkable] or 0)<m and (nd._clearance[self._walkable] or 0) or m
						m = (nr._clearance[self._walkable] or 0)<m and (nr._clearance[self._walkable] or 0) or m
						node._clearance[self._walkable] = m+1
					else
						node._clearance[self._walkable] = 1
					end
				else node._clearance[self._walkable] = 0
				end
			end
		end
		self._grid._isAnnotated[self._walkable] = true
		return self
	end

	--- Removes [clearance](http://aigamedev.com/open/tutorial/clearance-based-pathfinding/#TheTrueClearanceMetric)values.
	-- Clears cached clearance values for the current __walkable__.
  -- @class function
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage myFinder:clearAnnotations()
	function Pathfinder:clearAnnotations()
		assert(self._walkable, 'Finder must implement a walkable value')
		for node in self._grid:iter() do
			node:removeClearance(self._walkable)
		end
		self._grid._isAnnotated[self._walkable] = false
		return self
	end

  --- Sets the `grid`. Defines the given `grid` as the one on which the `pathfinder` will perform the search.
  -- @class function
  -- @tparam grid grid a `grid`
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage myFinder:setGrid(myGrid)
  function Pathfinder:setGrid(grid)
    assert(Assert.inherits(grid, Grid), 'Wrong argument #1. Expected a \'grid\' object')
    self._grid = grid
    self._grid._eval = self._walkable and type(self._walkable) == 'function'
    return self
  end

  --- Returns the `grid`. This is a reference to the actual `grid` used by the `pathfinder`.
  -- @class function
  -- @treturn grid the `grid`
	-- @usage local myGrid = myFinder:getGrid()
  function Pathfinder:getGrid()
    return self._grid
  end

  --- Sets the __walkable__ value or function.
  -- @class function
  -- @tparam string|int|func walkable the value for walkable nodes.
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage
	-- -- Value '0' is walkable
	-- myFinder:setWalkable(0)
	--
	-- -- Any value greater than 0 is walkable
	-- myFinder:setWalkable(function(n)
	--   return n>0
	-- end
  function Pathfinder:setWalkable(walkable)
    assert(Assert.matchType(walkable,'stringintfunctionnil'),
      ('Wrong argument #1. Expected \'string\', \'number\' or \'function\', got %s.'):format(type(walkable)))
    self._walkable = walkable
    self._grid._eval = type(self._walkable) == 'function'
    return self
  end

  --- Gets the __walkable__ value or function.
  -- @class function
  -- @treturn string|int|func the `walkable` value or function
	-- @usage local walkable = myFinder:getWalkable()
  function Pathfinder:getWalkable()
    return self._walkable
  end

  --- Defines the `finder`. It refers to the search algorithm used by the `pathfinder`.
  -- Default finder is `ASTAR`. Use @{Pathfinder:getFinders} to get the list of available finders.
  -- @class function
  -- @tparam string finderName the name of the `finder` to be used for further searches.
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage
	-- --To use Breadth-First-Search
	-- myFinder:setFinder('BFS')
	-- @see Pathfinder:getFinders
  function Pathfinder:setFinder(finderName)
		if not finderName then
			if not self._finder then
				finderName = 'ASTAR'
			else return
			end
		end
    assert(Finders[finderName],'Not a valid finder name!')
    self._finder = finderName
    return self
  end

  --- Returns the name of the `finder` being used.
  -- @class function
  -- @treturn string the name of the `finder` to be used for further searches.
	-- @usage local finderName = myFinder:getFinder()
  function Pathfinder:getFinder()
    return self._finder
  end

  --- Returns the list of all available finders names.
  -- @class function
  -- @treturn {string,...} array of built-in finders names.
	-- @usage
	-- local finders = myFinder:getFinders()
	-- for i, finderName in ipairs(finders) do
	--   print(i, finderName)
	-- end
  function Pathfinder:getFinders()
    return Utils.getKeys(Finders)
  end

  --- Sets a heuristic. This is a function internally used by the `pathfinder` to find the optimal path during a search.
  -- Use @{Pathfinder:getHeuristics} to get the list of all available `heuristics`. One can also define
  -- his own `heuristic` function.
  -- @class function
  -- @tparam func|string heuristic `heuristic` function, prototyped as __f(dx,dy)__ or as a `string`.
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
  -- @see Pathfinder:getHeuristics
	-- @see core.heuristics
	-- @usage myFinder:setHeuristic('MANHATTAN')
  function Pathfinder:setHeuristic(heuristic)
    assert(Heuristic[heuristic] or (type(heuristic) == 'function'),'Not a valid heuristic!')
    self._heuristic = Heuristic[heuristic] or heuristic
    return self
  end

  --- Returns the `heuristic` used. Returns the function itself.
  -- @class function
  -- @treturn func the `heuristic` function being used by the `pathfinder`
	-- @see core.heuristics
	-- @usage local h = myFinder:getHeuristic()
  function Pathfinder:getHeuristic()
    return self._heuristic
  end

  --- Gets the list of all available `heuristics`.
  -- @class function
  -- @treturn {string,...} array of heuristic names.
	-- @see core.heuristics
	-- @usage
	-- local heur = myFinder:getHeuristic()
	-- for i, heuristicName in ipairs(heur) do
	--   ...
	-- end
  function Pathfinder:getHeuristics()
    return Utils.getKeys(Heuristic)
  end

  --- Defines the search `mode`.
  -- The default search mode is the `DIAGONAL` mode, which implies 8-possible directions when moving (north, south, east, west and diagonals).
  -- In `ORTHOGONAL` mode, only 4-directions are allowed (north, south, east and west).
  -- Use @{Pathfinder:getModes} to get the list of all available search modes.
  -- @class function
  -- @tparam string mode the new search `mode`.
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
  -- @see Pathfinder:getModes
	-- @see Modes
	-- @usage myFinder:setMode('ORTHOGONAL')
  function Pathfinder:setMode(mode)
    assert(searchModes[mode],'Invalid mode')
    self._allowDiagonal = (mode == 'DIAGONAL')
    return self
  end

  --- Returns the search mode.
  -- @class function
  -- @treturn string the current search mode
	-- @see Modes
	-- @usage local mode = myFinder:getMode()
  function Pathfinder:getMode()
    return (self._allowDiagonal and 'DIAGONAL' or 'ORTHOGONAL')
  end

  --- Gets the list of all available search modes.
  -- @class function
  -- @treturn {string,...} array of search modes.
	-- @see Modes
	-- @usage local modes = myFinder:getModes()
	-- for modeName in ipairs(modes) do
	--   ...
	-- end
  function Pathfinder:getModes()
    return Utils.getKeys(searchModes)
  end

  --- Enables tunnelling. Defines the ability for the `pathfinder` to tunnel through walls when heading diagonally.
	-- This feature __is not compatible__ with Jump Point Search algorithm (i.e. enabling it will not affect Jump Point Search)
  -- @class function
  -- @tparam bool bool a boolean
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage myFinder:setTunnelling(true)
  function Pathfinder:setTunnelling(bool)
    assert(Assert.isBool(bool), ('Wrong argument #1. Expected boolean, got %s'):format(type(bool)))
		self._tunnel = bool
		return self
  end

  --- Returns tunnelling feature state.
  -- @class function
	-- @treturn bool tunnelling feature actual state
	-- @usage local isTunnellingEnabled = myFinder:getTunnelling()
  function Pathfinder:getTunnelling()
		return self._tunnel
  end

  --- Calculates a `path`. Returns the `path` from location __[startX, startY]__ to location __[endX, endY]__.
  -- Both locations must exist on the collision map. The starting location can be unwalkable.
  -- @class function
  -- @tparam int startX the x-coordinate for the starting location
  -- @tparam int startY the y-coordinate for the starting location
  -- @tparam int endX the x-coordinate for the goal location
  -- @tparam int endY the y-coordinate for the goal location
  -- @tparam int clearance the amount of clearance (i.e the pathing agent size) to consider
  -- @treturn path a path (array of nodes) when found, otherwise nil
	-- @usage local path = myFinder:getPath(1,1,5,5)
  function Pathfinder:getPath(startX, startY, endX, endY, clearance)
		self:reset()
    local startNode = self._grid:getNodeAt(startX, startY)
    local endNode = self._grid:getNodeAt(endX, endY)
    assert(startNode, ('Invalid location [%d, %d]'):format(startX, startY))
    assert(endNode and self._grid:isWalkableAt(endX, endY),
      ('Invalid or unreachable location [%d, %d]'):format(endX, endY))
    local _endNode = Finders[self._finder](self, startNode, endNode, clearance, toClear)
    if _endNode then
			return Utils.traceBackPath(self, _endNode, startNode)
    end
    return nil
  end

  --- Resets the `pathfinder`. This function is called internally between successive pathfinding calls, so you should not
	-- use it explicitely, unless under specific circumstances.
  -- @class function
	-- @treturn pathfinder self (the calling `pathfinder` itself, can be chained)
	-- @usage local path, len = myFinder:getPath(1,1,5,5)
	function Pathfinder:reset()
    for node in pairs(toClear) do node:reset() end
    toClear = {}
		return self
	end


  -- Returns Pathfinder class
	Pathfinder._VERSION = _VERSION
	Pathfinder._RELEASEDATE = _RELEASEDATE
  return setmetatable(Pathfinder,{
    __call = function(self,...)
      return self:new(...)
    end
  })

end
