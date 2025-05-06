local d = require("decimal")

a = d.new(1, 2) -- 100
b = d.new(3, 1) -- 30

print(a, "+", b, "=", a + b)
print(a, "-", b, "=", a - b)
print(a, "*", b, "=", a * b)
print(a, "/", b, "=", a / b)

x1 = d.new(1, 2)
x2 = d.new(2, 2)
x3 = d.new(2, 1)
x4 = d.new(10, 1)
x5 = d.new(10, 2)
print(a, "==", b, "=", a == b)
print(a, "==", x1, "=", a == x1)
print(a, "==", x2, "=", a == x2)
print(a, "==", x3, "=", a == x3)
print(a, "==", x4, "=", a == x4)
print(a, "==", x5, "=", a == x5)

assert(a + b == d.new(130, 0))
assert(a + b == d.new(13, 1))
assert(a - b == d.new(70, 0))
assert(a - b == d.new(7, 1))
assert(a * b == d.new(3000, 0))
assert(a * b == d.new(3, 3))
--assert(a / b == d.new(33333333333333, -15))

return a + b, a - b, a * b
