tk1 = { a = 1 }
tk2 = { b = 2 }

t = {}
t[tk1] = 111
t[tk2] = 222
t[t] = 333

return t[tk1], t[tk2], t[t]
