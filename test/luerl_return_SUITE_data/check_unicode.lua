function check_hun(erl_str)
    local lua_str = "árvíztűrő tükörfúrógép"
    return check_values(erl_str, lua_str, 31)
end

function check_lambda(erl_str)
    local lua_str = "λ"
    return check_values(erl_str, lua_str, 2)
end

function check_aquarius(erl_str)
    local lua_str = utf8.char(9810)
    return check_values(erl_str, lua_str, 3)
end

function check_values(erl_str, lua_str, length)
    assert(string.len(lua_str) == length, "invalid lua length")
    assert(string.len(erl_str) == length, "invalid erl length")
    assert(lua_str == erl_str, "different values")

    return erl_str, lua_str, erl_str == lua_str, string.len(erl_str), string.len(lua_str)
end
