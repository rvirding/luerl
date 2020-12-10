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


-- Quick example

Person = class('Person') --this is the same as class('Person', Object) or Object:subclass('Person')
function Person:initialize(name)
  self.name = name
end
function Person:speak()
  print('Hi, I am ' .. self.name ..'.')
end

AgedPerson = class('AgedPerson', Person) -- or Person:subclass('AgedPerson')
AgedPerson.static.ADULT_AGE = 18 --this is a class variable
function AgedPerson:initialize(name, age)
  Person.initialize(self, name) -- this calls the parent's constructor (Person.initialize) on self
  self.age = age
end
function AgedPerson:speak()
  Person.speak(self) -- prints "Hi, I am xx."
  if(self.age < AgedPerson.ADULT_AGE) then --accessing a class variable from an instance method
    print('I am underaged.')
  else
    print('I am an adult.')
  end
end

local p1 = AgedPerson:new('Billy the Kid', 13) -- this is equivalent to AgedPerson('Billy the Kid', 13) - the :new part is implicit
local p2 = AgedPerson:new('Luke Skywalker', 21)
p1:speak()
p2:speak()


-- Mixin

HasWings = { -- HasWings is a module, not a class. It can be "included" into classes
  fly = function(self)
    print('flap flap flap I am a ' .. self.class.name)
  end
}

Animal = class('Animal')

Insect = class('Insect', Animal) -- or Animal:subclass('Insect')

Worm = class('Worm', Insect) -- worms don't have wings

Bee = class('Bee', Insect)
Bee:include(HasWings) --Bees have wings. This adds fly() to Bee

Mammal = class('Mammal', Animal)

Fox = class('Fox', Mammal) -- foxes don't have wings, but are mammals

Bat = class('Bat', Mammal)
Bat:include(HasWings) --Bats have wings, too.

local bee = Bee() -- or Bee:new()
local bat = Bat() -- or Bat:new()
bee:fly()
bat:fly()

DrinksCoffee = {}

-- This is another valid way of declaring functions on a mixin.
-- Note that we are using the : operator, so there's an implicit self parameter
function DrinksCoffee:drink(drinkTime)
  if(drinkTime~=self.class.coffeeTime) then
    print(self.name .. ': It is not the time to drink coffee!')
  else
    print(self.name .. ': Mmm I love coffee at ' .. drinkTime)
  end
end

-- the included method is invoked every time DrinksCoffee is included on a class
-- notice that paramters can be passed around
function DrinksCoffee:included(klass)
  print(klass.name .. ' drinks coffee at ' .. klass.coffeeTime)
end

EnglishMan = class('EnglishMan')
EnglishMan.static.coffeeTime = 5
EnglishMan:include(DrinksCoffee)
function EnglishMan:initialize(name) self.name = name end

Spaniard = class('Spaniard')
Spaniard.static.coffeeTime = 6
Spaniard:include(DrinksCoffee)
function Spaniard:initialize(name) self.name = name end

tom = EnglishMan:new('tom')
juan = Spaniard:new('juan')

tom:drink(5)
juan:drink(5)
juan:drink(6)


-- Metamethods

Point = class('Point')
function Point:initialize(x,y)
  self.x = x
  self.y = y
end
function Point:__tostring()
  return 'Point: [' .. tostring(self.x) .. ', ' .. tostring(self.y) .. ']'
end

p1 = Point(100, 200)
p2 = Point(35, -10)
print(p1)
print(p2)