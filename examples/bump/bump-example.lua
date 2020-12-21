-- File    : bump-example.lua
-- Purpose : Brief demonstration of bump.lua on Luerl.
-- See     : ./bump.erl


local bump = require('bump')

-- The grid cell size can be specified via the initialize method
-- By default, the cell size is 64
local world = bump.newWorld(50)

-- create two rectangles
local A = {name="A"}
local B = {name="B"}

-- insert both rectangles into bump
world:add(A,   0, 0,    64, 256) -- x,y, width, height
world:add(B,   0, -100, 32, 32)

-- Try to move B to 0,64. If it collides with A, "slide over it"
local actualX, actualY, cols, len = world:move(B, 0,64)

-- prints "Attempted to move to 0,64, but ended up in 0,-32 due to 1 collisions"
if len > 0 then
  print(("Attempted to move to 0,64, but ended up in %d,%d due to %d collisions"):format(actualX, actualY, len))
else
  print("Moved B to 100,100 without collisions")
end

-- prints the new coordinates of B: 0, -32, 32, 32
print(world:getRect(B))

-- prints "Collision with A"
for i=1,len do -- If more than one simultaneous collision, they are sorted out by proximity
  local col = cols[i]
  print(("Collision with %s."):format(col.other.name))
end

-- remove A and B from the world
world:remove(A)
world:remove(B)