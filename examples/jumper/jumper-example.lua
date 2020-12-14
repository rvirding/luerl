-- Tests sample for clearance metrics calculation
-- See Figure 10 at http://aigamedev.com/open/tutorial/clearance-based-pathfinding/
-- Jump Point Search still has some flaws with clearance based pathfinding

local Grid = require 'jumper.grid'
local PF = require 'jumper.pathfinder'
local map = {
	{0,0,0,0,0,0,0,0,0,0},
	{0,0,0,0,0,0,0,0,1,0},
	{0,0,0,0,0,0,0,0,0,0},
	{0,0,0,1,0,0,0,0,0,0},
	{0,0,1,0,0,0,0,0,2,0},
	{0,0,1,1,1,0,0,2,0,0},
	{0,0,0,1,1,0,2,0,0,2},
	{0,0,0,0,1,0,0,0,0,2},
	{0,0,0,0,0,0,0,0,0,0},
	{0,0,0,0,0,0,0,0,0,0}
}
local grid = Grid(map)
local walkable = function(v) return v~=2 end
local finder = PF(grid, 'ASTAR',walkable)
finder:annotateGrid()
local finderNames = PF:getFinders()

local sx, sy = 1,1
local ex, ey = 9,9
local agent_size = 2

for i = 1,#finderNames do
	finder:setFinder(finderNames[i])
	local path = finder:getPath(sx, sy, ex, ey, agent_size)
	print(('Algorithm used: %s - Path %s')
		:format(finder:getFinder(), path and 'found' or 'not found'))
	if path then
		for node, count in path:nodes() do
			print(('  Step %d. (%d,%d)')
				:format(count, node:getPos()))
		end
	end
end
