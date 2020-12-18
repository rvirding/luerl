*Lua Fun: a high-performance functional programming library for Lua*

__Lua__ __Fun__ a high-performance functional programming library for Lua designed with JIT's trace compilers in mind.

Lua Fun provides programming primitives and high-order functions such as ``map``, ``filter``, ``reduce``, ``zip``, ..and many more, make it easy to **write simple and efficient functional code**.

Let's see an example:

    > -- Functional style
    > require "fun" ()
    > -- calculate sum(x for x^2 in 1..n)
    > n = 100
    > print(reduce(operator.add, 0, map(function(x) return x^2 end, range(n))))
    328350

    > -- Object-oriented style
    > local fun = require "fun"
    > -- calculate sum(x for x^2 in 1..n)
    > print(fun.range(n):map(function(x) return x^2 end):reduce(operator.add, 0))
    328350