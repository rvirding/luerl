-- Breadth-First search algorithm

if (...) then
  -- Internalization
  local t_remove = table.remove

  local function breadth_first_search(finder, openList, node, endNode, clearance, toClear)
    local neighbours = finder._grid:getNeighbours(node, finder._walkable, finder._allowDiagonal, finder._tunnel)
    for i = 1,#neighbours do
      local neighbour = neighbours[i]
      if not neighbour._closed and not neighbour._opened then
				local nClearance = neighbour._clearance[finder._walkable]
				local pushThisNode = clearance and nClearance and (nClearance >= clearance)			
        if (clearance and pushThisNode) or (not clearance) then
					openList[#openList+1] = neighbour
					neighbour._opened = true
					neighbour._parent = node
					toClear[neighbour] = true
				end
      end
    end

  end

  -- Calculates a path.
  -- Returns the path from location `<startX, startY>` to location `<endX, endY>`.
  return function (finder, startNode, endNode, clearance, toClear)

    local openList = {} -- We'll use a FIFO queue (simple array)
    openList[1] = startNode
    startNode._opened = true
    toClear[startNode] = true

    local node
    while (#openList > 0) do
      node = openList[1]
      t_remove(openList,1)
      node._closed = true
      if node == endNode then return node end
      breadth_first_search(finder, openList, node, endNode, clearance, toClear)
    end

    return nil
  end

end