-- Jump Point search algorithm

if (...) then

  -- Dependancies
  local _PATH = (...):match('(.+)%.search.jps$')
  local Heuristics = require (_PATH .. '.core.heuristics')
	local Heap = require (_PATH.. '.core.bheap')

  -- Internalization
  local max, abs = math.max, math.abs

  -- Local helpers, these routines will stay private
  -- As they are internally used by the public interface

  -- Resets properties of nodes expanded during a search
  -- This is a lot faster than resetting all nodes
  -- between consecutive pathfinding requests

  --[[
    Looks for the neighbours of a given node.
    Returns its natural neighbours plus forced neighbours when the given
    node has no parent (generally occurs with the starting node).
    Otherwise, based on the direction of move from the parent, returns
    neighbours while pruning directions which will lead to symmetric paths.

    In case diagonal moves are forbidden, when the given node has no
    parent, we return straight neighbours (up, down, left and right).
    Otherwise, we add left and right node (perpendicular to the direction
    of move) in the neighbours list.
  --]]
  local function findNeighbours(finder, node, clearance)

    if node._parent then
      local neighbours = {}
      local x,y = node._x, node._y
      -- Node have a parent, we will prune some neighbours
      -- Gets the direction of move
      local dx = (x-node._parent._x)/max(abs(x-node._parent._x),1)
      local dy = (y-node._parent._y)/max(abs(y-node._parent._y),1)

        -- Diagonal move case
      if dx~=0 and dy~=0 then
        local walkY, walkX

        -- Natural neighbours
        if finder._grid:isWalkableAt(x,y+dy,finder._walkable, clearance) then
          neighbours[#neighbours+1] = finder._grid:getNodeAt(x,y+dy)
          walkY = true
        end
        if finder._grid:isWalkableAt(x+dx,y,finder._walkable, clearance) then
          neighbours[#neighbours+1] = finder._grid:getNodeAt(x+dx,y)
          walkX = true
        end
        if walkX or walkY then
          neighbours[#neighbours+1] = finder._grid:getNodeAt(x+dx,y+dy)
        end

        -- Forced neighbours
        if (not finder._grid:isWalkableAt(x-dx,y,finder._walkable, clearance)) and walkY then
          neighbours[#neighbours+1] = finder._grid:getNodeAt(x-dx,y+dy)
        end
        if (not finder._grid:isWalkableAt(x,y-dy,finder._walkable, clearance)) and walkX then
          neighbours[#neighbours+1] = finder._grid:getNodeAt(x+dx,y-dy)
        end

      else
        -- Move along Y-axis case
        if dx==0 then
          local walkY
          if finder._grid:isWalkableAt(x,y+dy,finder._walkable, clearance) then
            neighbours[#neighbours+1] = finder._grid:getNodeAt(x,y+dy)

            -- Forced neighbours are left and right ahead along Y
            if (not finder._grid:isWalkableAt(x+1,y,finder._walkable, clearance)) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x+1,y+dy)
            end
            if (not finder._grid:isWalkableAt(x-1,y,finder._walkable, clearance)) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x-1,y+dy)
            end
          end
          -- In case diagonal moves are forbidden : Needs to be optimized
          if not finder._allowDiagonal then
            if finder._grid:isWalkableAt(x+1,y,finder._walkable, clearance) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x+1,y)
            end
            if finder._grid:isWalkableAt(x-1,y,finder._walkable, clearance)
              then neighbours[#neighbours+1] = finder._grid:getNodeAt(x-1,y)
            end
          end
        else
        -- Move along X-axis case
          if finder._grid:isWalkableAt(x+dx,y,finder._walkable, clearance) then
            neighbours[#neighbours+1] = finder._grid:getNodeAt(x+dx,y)

            -- Forced neighbours are up and down ahead along X
            if (not finder._grid:isWalkableAt(x,y+1,finder._walkable, clearance)) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x+dx,y+1)
            end
            if (not finder._grid:isWalkableAt(x,y-1,finder._walkable, clearance)) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x+dx,y-1)
            end
          end
          -- : In case diagonal moves are forbidden
          if not finder._allowDiagonal then
            if finder._grid:isWalkableAt(x,y+1,finder._walkable, clearance) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x,y+1)
            end
            if finder._grid:isWalkableAt(x,y-1,finder._walkable, clearance) then
              neighbours[#neighbours+1] = finder._grid:getNodeAt(x,y-1)
            end
          end
        end
      end
      return neighbours
    end

    -- Node do not have parent, we return all neighbouring nodes
    return finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel, clearance)
  end

  --[[
    Searches for a jump point (or a turning point) in a specific direction.
    This is a generic translation of the algorithm 2 in the paper:
      http://users.cecs.anu.edu.au/~dharabor/data/papers/harabor-grastien-aaai11.pdf
    The current expanded node is a jump point if near a forced node

    In case diagonal moves are forbidden, when lateral nodes (perpendicular to
    the direction of moves are walkable, we force them to be turning points in other
    to perform a straight move.
  --]]
  local function jump(finder, node, parent, endNode, clearance)
	if not node then return end

    local x,y = node._x, node._y
    local dx, dy = x - parent._x,y - parent._y

    -- If the node to be examined is unwalkable, return nil
    if not finder._grid:isWalkableAt(x,y,finder._walkable, clearance) then return end
		
    -- If the node to be examined is the endNode, return this node
    if node == endNode then return node end
    -- Diagonal search case
    if dx~=0 and dy~=0 then
      -- Current node is a jump point if one of his leftside/rightside neighbours ahead is forced
      if (finder._grid:isWalkableAt(x-dx,y+dy,finder._walkable, clearance) and (not finder._grid:isWalkableAt(x-dx,y,finder._walkable, clearance))) or
         (finder._grid:isWalkableAt(x+dx,y-dy,finder._walkable, clearance) and (not finder._grid:isWalkableAt(x,y-dy,finder._walkable, clearance))) then
        return node
      end
    else
      -- Search along X-axis case
      if dx~=0 then
        if finder._allowDiagonal then
          -- Current node is a jump point if one of his upside/downside neighbours is forced
          if (finder._grid:isWalkableAt(x+dx,y+1,finder._walkable, clearance) and (not finder._grid:isWalkableAt(x,y+1,finder._walkable, clearance))) or
             (finder._grid:isWalkableAt(x+dx,y-1,finder._walkable, clearance) and (not finder._grid:isWalkableAt(x,y-1,finder._walkable, clearance))) then
            return node
          end
        else
          -- : in case diagonal moves are forbidden
          if finder._grid:isWalkableAt(x+1,y,finder._walkable, clearance) or finder._grid:isWalkableAt(x-1,y,finder._walkable, clearance) then return node end
        end
      else
      -- Search along Y-axis case
        -- Current node is a jump point if one of his leftside/rightside neighbours is forced
        if finder._allowDiagonal then
          if (finder._grid:isWalkableAt(x+1,y+dy,finder._walkable, clearance) and (not finder._grid:isWalkableAt(x+1,y,finder._walkable, clearance))) or
             (finder._grid:isWalkableAt(x-1,y+dy,finder._walkable, clearance) and (not finder._grid:isWalkableAt(x-1,y,finder._walkable, clearance))) then
            return node
          end
        else
          -- : in case diagonal moves are forbidden
          if finder._grid:isWalkableAt(x,y+1,finder._walkable, clearance) or finder._grid:isWalkableAt(x,y-1,finder._walkable, clearance) then return node end
        end
      end
    end

    -- Recursive horizontal/vertical search
    if dx~=0 and dy~=0 then
      if jump(finder,finder._grid:getNodeAt(x+dx,y),node,endNode, clearance) then return node end
      if jump(finder,finder._grid:getNodeAt(x,y+dy),node,endNode, clearance) then return node end
    end

    -- Recursive diagonal search
    if finder._allowDiagonal then
      if finder._grid:isWalkableAt(x+dx,y,finder._walkable, clearance) or finder._grid:isWalkableAt(x,y+dy,finder._walkable, clearance) then
        return jump(finder,finder._grid:getNodeAt(x+dx,y+dy),node,endNode, clearance)
      end
    end
end

  --[[
    Searches for successors of a given node in the direction of each of its neighbours.
    This is a generic translation of the algorithm 1 in the paper:
      http://users.cecs.anu.edu.au/~dharabor/data/papers/harabor-grastien-aaai11.pdf

    Also, we notice that processing neighbours in a reverse order producing a natural
    looking path, as the pathfinder tends to keep heading in the same direction.
    In case a jump point was found, and this node happened to be diagonal to the
    node currently expanded in a straight mode search, we skip this jump point.
  --]]
  local function identifySuccessors(finder, openList, node, endNode, clearance, toClear)

    -- Gets the valid neighbours of the given node
    -- Looks for a jump point in the direction of each neighbour
    local neighbours = findNeighbours(finder,node, clearance)
    for i = #neighbours,1,-1 do

      local skip = false
      local neighbour = neighbours[i]
      local jumpNode = jump(finder,neighbour,node,endNode, clearance)
		
      -- : in case a diagonal jump point was found in straight mode, skip it.
      if jumpNode and not finder._allowDiagonal then
        if ((jumpNode._x ~= node._x) and (jumpNode._y ~= node._y)) then skip = true end
      end
		
      -- Performs regular A-star on a set of jump points
      if jumpNode and not skip then
        -- Update the jump node and move it in the closed list if it wasn't there
        if not jumpNode._closed then			
					local extraG = Heuristics.EUCLIDIAN(jumpNode, node)
					local newG = node._g + extraG
					if not jumpNode._opened or newG < jumpNode._g then
						toClear[jumpNode] = true -- Records this node to reset its properties later.
						jumpNode._g = newG
						jumpNode._h = jumpNode._h or
							(finder._heuristic(jumpNode, endNode))
						jumpNode._f = jumpNode._g+jumpNode._h
						jumpNode._parent = node
						if not jumpNode._opened then
							openList:push(jumpNode)
							jumpNode._opened = true
						else
							openList:heapify(jumpNode)
						end
					end					
				end
      end
    end
  end

  -- Calculates a path.
  -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
  return function(finder, startNode, endNode, clearance, toClear)

    startNode._g, startNode._f, startNode._h = 0,0,0
		local openList = Heap()
    openList:push(startNode)
    startNode._opened = true
    toClear[startNode] = true

    local node
    while not openList:empty() do
      -- Pops the lowest F-cost node, moves it in the closed list
      node = openList:pop()
      node._closed = true
        -- If the popped node is the endNode, return it
        if node == endNode then
          return node
        end
      -- otherwise, identify successors of the popped node
      identifySuccessors(finder, openList, node, endNode, clearance, toClear)
    end

    -- No path found, return nil
    return nil
  end

end