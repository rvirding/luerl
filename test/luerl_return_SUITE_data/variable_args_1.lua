function get_from_table(Map, Key, ...)
    local Value = Map[Key]
    if select('#', ...) > 0 then
        return get_from_table(Value, ...)
    else
        return Value
    end
end

local tab = { a = { x1 = { x2 = 99 } }, b = 88, c = { d = 77 } }

return
get_from_table(tab, "a", "x1", "x2"),
get_from_table(tab, "b"),
get_from_table(tab, "c", "d")
