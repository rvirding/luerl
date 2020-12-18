__Lua__ __Fun__ a high-performance functional programming library for Lua designed with LuaJIT's trace compiler in mind.

<img src="/examples/fun/luafun.png" align="right" width="174px" height="144px" />

Lua Fun provides programming primitives and high-order functions such as ``map``, ``filter``, ``reduce``, ``zip``, ..etc, make it easy to **write simple and efficient functional code**.

Let's see an example:

```lua
    -- Functional style
    for k, v in pairs(require("fun")) do _G[k] = v end
    -- calculate sum(x for x^2 in 1..n)
    n = 100
    print(reduce(operator.add, 0, map(function(x) return x^2 end, range(n))))
    328350
    
    -- Object-oriented style
    local fun = require("fun")
    -- calculate sum(x for x^2 in 1..n)
    print(fun.range(n):map(function(x) return x^2 end):reduce(operator.add, 0))
    328350
```

Let's shed some light on the internal library structure and working
principles.

## Iterators

A basic primitive of the library after functions is an iterator. Most functions
takes an iterator and returns a new iteraror(s). Iterators all the way down!

The simplest iterator is (surprise!) :func:`pairs` and :func:`ipairs`
Lua functions. Have you ever tried to call, say, :func:`ipairs` function
without using it inside a ``for`` loop? Try to do that on any Lua
implementation:

```lua
    ipairs({'a', 'b', 'c'})
    function: builtin#6     table: 0x40f80e38       0
```
The function returned three strange values which look useless without a ``for``
loop. We call these values **iterator triplet**.
Let's see how each value is used for:

``gen`` -- first value
   A generating function that can produce a next value on each iteration.
   Usually returns a new ``state`` and iteration values (multireturn).

``param`` -- second value
   A permanent (constant) parameter of a generating function is used to create
   specific instance of the generating function. For example, a table itself
   for ``ipairs`` case.

``state`` -- third value
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

**Please do not panic!** You do not have to use these values directly.
It is just a nice trick to get ``for .. in`` loop working in Lua.
