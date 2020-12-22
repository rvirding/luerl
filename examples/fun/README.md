__Lua Fun__: A high-performance functional programming library for Lua designed with LuaJIT's trace compiler in mind.

<img src="/examples/fun/fun.png" align="right" width="174px" height="144px" />

__Tested on Luerl 5.3 and everything works__, check out `luafun-example.lua`, read the __Lua Fun__ API [reference](https://luafun.github.io/reference.html)!

## Installation

Drop the file [fun.lua](https://raw.githubusercontent.com/luafun/luafun/master/fun.lua) into your project and add it to your code with the require function:

```lua
-- Object-oriented style
local fun = require("fun")
-- calculate sum(x for x^2 in 1..n)
print(fun.range(n):map(function(x) return x^2 end):reduce(operator.add, 0))
328350

-- Functional style
for k, v in pairs(require("fun")) do _G[k] = v end
-- calculate sum(x for x^2 in 1..n)
n = 100
print(reduce(operator.add, 0, map(function(x) return x^2 end, range(n))))
328350
```

## Iterators

A basic primitive of the library after functions is an iterator. Most functions
takes an iterator and returns a new iteraror([s](https://en.wikipedia.org/wiki/Turtles_all_the_way_down)).

The simplest iterator is (surprise!) `pairs` and `ipairs`
Lua functions. Have you ever tried to call, say, `ipairs` function
without using it inside a ``for`` loop? Try to do that on any Lua
implementation:

```lua
    ipairs({'a', 'b', 'c'})
    function: builtin#6     table: 0x40f80e38       0
```
The function returned three strange values which look useless without a ``for``
loop. We call these values **iterator triplet**.
Let's see how each value is used for:

``gen`` -- first value<br>
   A generating function that can produce a next value on each iteration.
   Usually returns a new ``state`` and iteration values (multireturn).

``param`` -- second value<br>
   A permanent (constant) parameter of a generating function is used to create
   specific instance of the generating function. For example, a table itself
   for ``ipairs`` case.

``state`` -- third value<br>
   A some transient state of an iterator that is changed after each iteration.
   For example, an array index for ``ipairs`` case.

Try to call ``gen`` function manually:

```lua
    gen, param, state = ipairs({'a', 'b', 'c'})
    print(gen(param, state))
    1       a
```
The ``gen`` function returned a new state ``1`` and the next iteration
value ``a``. The second call to ``gen`` with the new state will return the next
state  and the next iteration value. When the iterator finishes to the end
the ``nil`` value is returned instead of the next state.

**Do not panic!** You do not have to use these values directly.
It is just a nice trick to get ``for .. in`` loop working in Lua.

## Iterations

What happens when you type the following code into a Lua console:

```lua
    for _it, x in ipairs({'a', 'b', 'c'}) do print(x) end
```
According to [Lua reference manual](https://www.lua.org/manual/5.3/manual.html#3.3.5) for the code above is equivalent to:
```lua
    do
        -- Initialize the iterator
        local gen, param, state = ipairs({'a', 'b', 'c'})
        while true do
            -- Next iteration
            local state, var_1, ···, var_n = gen(param, state)
            if state == nil then break end
            -- Assign values to our variables
            _it = state
            x = var_1
            -- Execute the code block
            print(x)
        end
    end
```
What does it mean for us?

* An iterator can be used together with ``for .. in`` to generate a loop
* An iterator is fully defined using ``gen``, ``param`` and ``state`` iterator
  triplet
* The ``nil`` state marks the end of an iteration
* An iterator can return an arbitrary number of values (multireturn)
* It is possible to make some wrapping functions to take an iterator and

  return a new modified iterator

**The library provides a set of iterators** that can be used like ``pairs``
and ``ipairs``.

## Iterator Types

### Pure functional iterators

Iterators can be either pure functional or have some side effects and returns
different values for some initial conditions. An **[iterator is
pure functional](https://en.wikipedia.org/wiki/Pure_function)** if it meets the following criteria:

- ``gen`` function always returns the same values for the same ``param`` and
  ``state`` values (idempotence property)
- ``param`` and ``state`` values are not modified during ``gen`` call and
  a new ``state`` object is returned instead (referential transparency
  property).

Pure functional iterators are very important for us. Pure functional iterator
can be safety cloned or reapplied without creating side effects. Many library
function use these properties.

### Finite iterators

Iterators can be **finite** (sooner or later end up) or **infinite**
(never end).
Since there is [no way](https://en.wikipedia.org/wiki/Halting_problem) to determine automatically if an iterator is finite or
not the library function can not automatically resolve infinite
loops. It is your obligation to do not pass infinite iterator to reducing
functions.

## License

This work is under [MIT-LICENSE](http://www.opensource.org/licenses/mit-license.php)<br/>

Copyright (c) 2013-2021 Roman Tsisyk <br/>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
