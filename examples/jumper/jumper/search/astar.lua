-- Astar algorithm
-- This actual implementation of A-star is based on
-- [Nash A. & al. pseudocode](http://aigamedev.com/open/tutorials/theta-star-any-angle-paths/)

if (...) then

	-- Internalization
	local ipairs = ipairs
	local huge = math.huge

	-- Dependancies
	local _PATH = (...):match('(.+)%.search.astar$')
	local Heuristics = require (_PATH .. '.core.heuristics')
	local Heap = require (_PATH.. '.core.bheap')

	-- Updates G-cost
	local function computeCost(node, neighbour, finder, clearance)
		local mCost = Heuristics.EUCLIDIAN(neighbour, node)
		if node._g + mCost < neighbour._g then
			neighbour._parent = node
			neighbour._g = node._g + mCost
		end
	end

	-- Updates vertex node-neighbour
	local function updateVertex(finder, openList, node, neighbour, endNode, clearance, heuristic, overrideCostEval)
		local oldG = neighbour._g
		local cmpCost = overrideCostEval or computeCost
		cmpCost(node, neighbour, finder, clearance)
		if neighbour._g < oldG then
			local nClearance = neighbour._clearance[finder._walkable]
			local pushThisNode = clearance and nClearance and (nClearance >= clearance)
			if (clearance and pushThisNode) or (not clearance) then
				if neighbour._opened then neighbour._opened = false end				
				neighbour._h = heuristic(endNode, neighbour)
				neighbour._f = neighbour._g + neighbour._h
				openList:push(neighbour)
				neighbour._opened = true
			end
		end
	end

  -- Calculates a path.
  -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
  return function (finder, startNode, endNode, clearance, toClear, overrideHeuristic, overrideCostEval)
		
		local heuristic = overrideHeuristic or finder._heuristic
		local openList = Heap()
		startNode._g = 0
		startNode._h = heuristic(endNode, startNode)
		startNode._f = startNode._g + startNode._h
		openList:push(startNode)
		toClear[startNode] = true
		startNode._opened = true

		while not openList:empty() do
			local node = openList:pop()
			node._closed = true
			if node == endNode then return node end
			local neighbours = finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel)
			for i = 1,#neighbours do
				local neighbour = neighbours[i]
				if not neighbour._closed then
					toClear[neighbour] = true
					if not neighbour._opened then
						neighbour._g = huge
						neighbour._parent = nil	
					end
					updateVertex(finder, openList, node, neighbour, endNode, clearance, heuristic, overrideCostEval)
				end	
			end	
		end
		
		return nil 
	end

end
