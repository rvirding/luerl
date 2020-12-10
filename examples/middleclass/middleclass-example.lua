-- File    : middleclass-example.lua
-- Purpose : Brief demonstration of middleclass on Luerl.
-- See     : ./middleclass.erl


local class = require('middleclass')

local Fruit = class('Fruit') -- 'Fruit' is the class' name

function Fruit:initialize(sweetness)
  self.sweetness = sweetness
end

Fruit.static.sweetness_threshold = 5 -- class variable (also admits methods)

function Fruit:isSweet()
  return self.sweetness > Fruit.sweetness_threshold
end

local Lemon = class('Lemon', Fruit) -- subclassing

function Lemon:initialize()
  Fruit.initialize(self, 1) -- invoking the superclass' initializer
end

local lemon = Lemon:new()

print(lemon:isSweet()) -- false
