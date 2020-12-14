--- A light implementation of Binary heaps data structure.
-- While running a search, some search algorithms (Astar, Dijkstra, Jump Point Search) have to maintains
-- a list of nodes called __open list__. Retrieve from this list the lowest cost node can be quite slow, 
-- as it normally requires to skim through the full set of nodes stored in this list. This becomes a real 
-- problem especially when dozens of nodes are being processed (on large maps). 
--
-- The current module implements a <a href="http://www.policyalmanac.org/games/binaryHeaps.htm">binary heap</a>
-- data structure, from which the search algorithm will instantiate an open list, and cache the nodes being 
-- examined during a search. As such, retrieving the lower-cost node is faster and globally makes the search end 
-- up quickly.
-- 
-- This module is internally used by the library on purpose.
-- It should normally not be used explicitely, yet it remains fully accessible.
--

--[[
  Notes:
  This lighter implementation of binary heaps, based on :
    https://github.com/Yonaba/Binary-Heaps
--]]

if (...) then

	-- Dependency
	local Utils = require((...):gsub('%.bheap$','.utils'))
	
	-- Local reference
	local floor = math.floor

	-- Default comparison function
	local function f_min(a,b) return a < b end

	-- Percolates up
	local function percolate_up(heap, index)
		if index == 1 then return end
		local pIndex
		if index <= 1 then return end
		if index%2 == 0 then
			pIndex =  index/2
		else pIndex = (index-1)/2
		end
		if not heap._sort(heap._heap[pIndex], heap._heap[index]) then
			heap._heap[pIndex], heap._heap[index] = 
				heap._heap[index], heap._heap[pIndex]
			percolate_up(heap, pIndex)
		end
	end

	-- Percolates down
	local function percolate_down(heap,index)
		local lfIndex,rtIndex,minIndex
		lfIndex = 2*index
		rtIndex = lfIndex + 1
		if rtIndex > heap._size then
			if lfIndex > heap._size then return
			else minIndex = lfIndex  end
		else
			if heap._sort(heap._heap[lfIndex],heap._heap[rtIndex]) then
				minIndex = lfIndex
			else
				minIndex = rtIndex
			end
		end
		if not heap._sort(heap._heap[index],heap._heap[minIndex]) then
			heap._heap[index],heap._heap[minIndex] = heap._heap[minIndex],heap._heap[index]
			percolate_down(heap,minIndex)
		end
	end

	-- Produces a new heap
	local function newHeap(template,comp)
		return setmetatable({_heap = {},
			_sort = comp or f_min, _size = 0},
		template)
	end


	--- The `heap` class.<br/>
	-- This class is callable.
	-- _Therefore,_ <code>heap(...)</code> _is used to instantiate new heaps_.
	-- @type heap
	local heap = setmetatable({},
		{__call = function(self,...)
			return newHeap(self,...)
		end})
	heap.__index = heap

	--- Checks if a `heap` is empty
	-- @class function
	-- @treturn bool __true__ of no item is queued in the heap, __false__ otherwise
	-- @usage
	-- if myHeap:empty() then 
	--   print('Heap is empty!')
	-- end
	function heap:empty()
		return (self._size==0)
	end

	--- Clears the `heap` (removes all items queued in the heap)
	-- @class function
	-- @treturn heap self (the calling `heap` itself, can be chained)
	-- @usage myHeap:clear()
	function heap:clear()
		self._heap = {}
		self._size = 0
		self._sort = self._sort or f_min
		return self
	end

	--- Adds a new item in the `heap`
	-- @class function
	-- @tparam value item a new value to be queued in the heap
	-- @treturn heap self (the calling `heap` itself, can be chained)
	-- @usage
	-- myHeap:push(1)
	-- -- or, with chaining
	-- myHeap:push(1):push(2):push(4)
	function heap:push(item)
		if item then
			self._size = self._size + 1
			self._heap[self._size] = item
			percolate_up(self, self._size)
		end
		return self
	end

	--- Pops from the `heap`.
	-- Removes and returns the lowest cost item (with respect to the comparison function being used) from the `heap`.
	-- @class function
	-- @treturn value a value previously pushed into the heap
	-- @usage
	-- while not myHeap:empty() do 
	--   local lowestValue = myHeap:pop()
	--   ...
	-- end
	function heap:pop()
		local root
		if self._size > 0 then
			root = self._heap[1]
			self._heap[1] = self._heap[self._size]
			self._heap[self._size] = nil
			self._size = self._size-1
			if self._size>1 then
				percolate_down(self, 1)
			end
		end
		return root
	end

	--- Restores the `heap` property.
	-- Reorders the `heap` with respect to the comparison function being used. 
	-- When given argument __item__ (a value existing in the `heap`), will sort from that very item in the `heap`. 
	-- Otherwise, the whole `heap` will be cheacked. 
	-- @class function
	-- @tparam[opt] value item the modified value
	-- @treturn heap self (the calling `heap` itself, can be chained)
	-- @usage myHeap:heapify() 
	function heap:heapify(item)
		if self._size == 0 then return end
		if item then
			local i = Utils.indexOf(self._heap,item)
			if i then 
				percolate_down(self, i)
				percolate_up(self, i)
			end
			return
		end
		for i = floor(self._size/2),1,-1 do
			percolate_down(self,i)
		end
		return self
	end

	return heap
end