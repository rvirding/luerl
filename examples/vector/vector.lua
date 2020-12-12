local module = {
  _version = "vector.lua v2019.14.12",
  _description = "a simple vector library for Lua based on the PVector class from processing",
  _url = "https://github.com/themousery/vector.lua",
  _license = [[
    Copyright (c) 2018 themousery

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
  ]]
}

-- create the module
local vector = {}
vector.__index = vector

-- get a random function from Love2d or base lua, in that order.
local rand = math.random
if love and love.math then rand = love.math.random end

-- makes a new vector
local function new(x,y)
  return setmetatable({x=x or 0, y=y or 0}, vector)
end

-- makes a new vector from an angle
local function fromAngle(theta)
  return new(math.cos(theta), -math.sin(theta))
end

-- makes a vector with a random direction
local function random()
  return fromAngle(rand() * math.pi*2)
end

-- check if an object is a vector
local function isvector(t)
  return getmetatable(t) == vector
end

-- set the values of the vector to something new
function vector:set(x,y)
  if isvector(x) then self.x, self.y = x.x, x.y;return end
  self.x, self.y = x or self.x, y or self.y
  return self
end

-- replace the values of a vector with the values of another vector
function vector:replace(v)
  assert(isvector(v), "replace: wrong argument type: (expected <vector>, got "..type(v)..")")
  self.x, self.y = v.x, v.y
  return self
end

-- returns a copy of a vector
function vector:clone()
  return new(self.x, self.y)
end

-- get the magnitude of a vector
function vector:getmag()
  return math.sqrt(self.x^2 + self.y^2)
end

-- get the magnitude squared of a vector
function vector:magSq()
  return self.x^2 + self.y^2
end

-- set the magnitude of a vector
function vector:setmag(mag)
  self:norm()
  local v = self * mag
  self:replace(v)
  return self
end

-- meta function to make vectors negative
-- ex: (negative) -vector(5,6) is the same as vector(-5,-6)
function vector.__unm(v)
  return new(-v.x, -v.y)
end

-- meta function to add vectors together
-- ex: (vector(5,6) + vector(6,5)) is the same as vector(11,11)
function vector.__add(a,b)
  assert(isvector(a) and isvector(b), "add: wrong argument types: (expected <vector> and <vector>)")
  return new(a.x+b.x, a.y+b.y)
end

-- meta function to subtract vectors
function vector.__sub(a,b)
  assert(isvector(a) and isvector(b), "sub: wrong argument types: (expected <vector> and <vector>)")
  return new(a.x-b.x, a.y-b.y)
end

-- meta function to multiply vectors
function vector.__mul(a,b)
  if type(a) == 'number' then 
    return new(a * b.x, a * b.y)
  elseif type(b) == 'number' then
    return new(a.x * b, a.y * b)
  else
    assert(isvector(a) and isvector(b),  "mul: wrong argument types: (expected <vector> or <number>)")
    return new(a.x*b.x, a.y*b.y)
  end
end

-- meta function to divide vectors
function vector.__div(a,b)
  assert(isvector(a) and type(b) == "number", "div: wrong argument types (expected <vector> and <number>)")
  return new(a.x/b, a.y/b)
end

-- meta function to check if vectors have the same values
function vector.__eq(a,b)
  assert(isvector(a) and isvector(b), "eq: wrong argument types (expected <vector> and <vector>)")
  return a.x==b.x and a.y==b.y
end

-- meta function to change how vectors appear as string
-- ex: print(vector(2,8)) - this prints '(2,8)'
function vector:__tostring()
  return "("..self.x..", "..self.y..")"
end

-- get the distance between two vectors
function vector.dist(a,b)
  assert(isvector(a) and isvector(b), "dist: wrong argument types (expected <vector> and <vector>)")
  return math.sqrt((a.x-b.x)^2 + (a.y-b.y)^2)
end

-- return the dot product of the vector
function vector:dot(v)
  assert(isvector(v), "dot: wrong argument type (expected <vector>)")
  return self.x * v.x + self.y * v.y
end

-- normalize the vector (give it a magnitude of 1)
function vector:norm()
  local m = self:getmag()
  if m~=0 then
    self:replace(self / m)
  end
  return self
end

-- limit the vector to a certain amount
function vector:limit(max)
  assert(type(max) == 'number', "limit: wrong argument type (expected <number>)")
  local mSq = self:magSq()
  if mSq > max^2 then
    self:setmag(max)
  end
  return self
end

-- Clamp each axis between max and min's corresponding axis
function vector:clamp(min, max)
  assert(isvector(min) and isvector(max), "clamp: wrong argument type (expected <vector>) and <vector>")
  local x = math.min( math.max( self.x, min.x ), max.x )
  local y = math.min( math.max( self.y, min.y ), max.y )
  self:set(x,y)
  return self
end

-- get the heading (direction) of a vector
function vector:heading()
  return -math.atan2(self.y, self.x)
end

-- rotate a vector by a certain number of degrees
function vector:rotate(theta)
  local m = self:getmag()
  self:replace(fromAngle(self:heading() + theta))
  self:setmag(m)
  return self
end

-- return x and y of vector as a regular array
function vector:array()
  return {self.x, self.y}
end

-- return x and y of vector, unpacked from table
function vector:unpack()
  return self.x, self.y
end


-- pack up and return module
module.new = new
module.random = random
module.fromAngle = fromAngle
module.isvector = isvector
return setmetatable(module, {__call = function(_,...) return new(...) end})
