local tonumber = tonumber
local math_huge = math.huge

bench = {}

-- TODO: handle infinity
-- bench.e309 = function()
--  local inf = 1e309
-- end

-- bench.huge = function()
--  local inf = math_huge
-- end

-- bench.divide = function()
--  local inf = 1/0
-- end

-- bench.tonumber = function()
--  local inf = tonumber("inf")
-- end

return bench
