local Decimal = {}

Decimal.mt = {
    __tostring = function(d)
        return d.b .. "e" .. d.e
    end,

    __add = function(a, b)
        a, b = Decimal.same_e(a, b)

        if a.e == b.e then
            return Decimal.new(a.b + b.b, a.e)
        end
    end,
    __sub = function(a, b)
        return a + Decimal.new(-b.b, b.e)
    end,
    __mul = function(a, b)
        return Decimal.new(a.b * b.b, a.e + b.e)
    end,
    __div = function(a, b)
        -- @todo fix
        return Decimal.new(a.b / b.b, a.e - b.e)
    end,

    __eq = function(a, b)
        a, b = Decimal.same_e(a, b)
        return a.b == b.b and a.e == b.e
    end
}

Decimal.new = function(b, e)
    local d = {
        b = b,
        e = e
    }
    setmetatable(d, Decimal.mt)
    return d
end

Decimal.change_e = function(d, e)
    if d.e == e then
        return d
    end
    if d.e > e then
        return Decimal.change_e(Decimal.new(10 * d.b, d.e - 1), e)
    end
    error("can't increment exponential")
end

Decimal.same_e = function(a, b)
    local min_e = math.min(a.e, b.e)
    return Decimal.change_e(a, min_e), Decimal.change_e(b, min_e)
end

return Decimal
