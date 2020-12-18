__Lua__ __Fun__ a high-performance functional programming library for Lua designed with LuaJIT's trace compiler in mind.

<img src="/examples/fun/luafun.png" align="right" width="174px" height="144px" />

Lua Fun provides programming primitives and high-order functions such as ``map``, ``filter``, ``reduce``, ``zip``, ..etc, make it easy to **write simple and efficient functional code**.

Let's see an example:

    > -- Functional style
    > for k, v in pairs(require("fun")) do _G[k] = v end
    > -- calculate sum(x for x^2 in 1..n)
    > n = 100
    > print(reduce(operator.add, 0, map(function(x) return x^2 end, range(n))))
    328350

    > -- Object-oriented style
    > local fun = require("fun")
    > -- calculate sum(x for x^2 in 1..n)
    > print(fun.range(n):map(function(x) return x^2 end):reduce(operator.add, 0))
    328350
