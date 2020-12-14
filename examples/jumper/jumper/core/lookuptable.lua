local addNode(self, node, nextNode, ed)
	if not self._pathDB[node] then self._pathDB[node] = {} end
	self._pathDB[node][ed] = (nextNode == ed and node or nextNode)
end

-- Path lookupTable
local lookupTable = {}
lookupTable.__index = lookupTable

function lookupTable:new()
	local lut = {_pathDB = {}}
	return setmetatable(lut, lookupTable)
end

function lookupTable:addPath(path)
	local st, ed = path._nodes[1], path._nodes[#path._nodes]
	for node, count in path:nodes() do
		local nextNode = path._nodes[count+1]
		if nextNode then addNode(self, node, nextNode, ed) end
	end
end

function lookupTable:hasPath(nodeA, nodeB)
	local found
	found = self._pathDB[nodeA] and self._path[nodeA][nodeB]
	if found then return true, true end
	found = self._pathDB[nodeB] and self._path[nodeB][nodeA]
	if found then return true, false end
	return false
end

return lookupTable