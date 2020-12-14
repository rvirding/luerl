-- ThetaStar implementation
-- See: http://aigamedev.com/open/tutorials/theta-star-any-angle-paths for reference

if (...) then
	
	local _PATH = (...):gsub('%.search.thetastar$','')

	-- Depandancies
	local Heuristics   = require (_PATH .. '.core.heuristics')
	local astar_search = require (_PATH .. '.search.astar')

	-- Internalization
	local ipairs = ipairs
	local huge, abs = math._huge, math.abs
	
	-- Line Of Sight (Bresenham's line marching algorithm)
	-- http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
	local lineOfSight = function (node, neighbour, finder, clearance)
		local x0, y0 = node._x, node._y
		local x1, y1 = neighbour._x, neighbour._y
		local dx = abs(x1-x0)
		local dy = abs(y1-y0)
		local err = dx - dy
		local sx = (x0 < x1) and 1 or -1
		local sy = (y0 < y1) and 1 or -1		

		while true do
			if not finder._grid:isWalkableAt(x0, y0, finder._walkable, finder._tunnel, clearance) then 
				return false 
			end
			if x0 == x1 and y0 == y1 then
				break
			end
			local e2 = 2*err
			if e2 > -dy then
				err = err - dy
				x0 = x0 + sx
			end
			if e2 < dx then
				err = err + dx
				y0 = y0 + sy
			end
		end
		return true
	end
	
	-- Theta star cost evaluation
	local function computeCost(node, neighbour, finder, clearance)
		local parent = node._parent or node
		local mpCost = Heuristics.EUCLIDIAN(neighbour, parent)
		if lineOfSight(parent, neighbour, finder, clearance) then
			if parent._g + mpCost < neighbour._g then
				neighbour._parent = parent
				neighbour._g = parent._g + mpCost
			end
		else
			local mCost = Heuristics.EUCLIDIAN(neighbour, node)
			if node._g + mCost < neighbour._g then
				neighbour._parent = node
				neighbour._g = node._g + mCost
			end
		end
	end

  -- Calculates a path.
  -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
  return function (finder, startNode, endNode, clearance, toClear, overrideHeuristic)
    return astar_search(finder, startNode, endNode, clearance, toClear, overrideHeuristic, computeCost)
	end

end