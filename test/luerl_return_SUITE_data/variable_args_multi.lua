function make_table(...)
    local t = {}
    for i = 1, select('#', ...), 2 do
        k = select(i, ...)
        v = select(i + 1, ...)
        t[k] = v
    end
    return t
end

print(make_table)

local tab = make_table("x", 9, 7, "banana", "z", 8)

assert(tab["x"] == 9)
assert(tab[7] == "banana")
assert(tab.z == 8)

return tab["x"], tab[7], tab.z
