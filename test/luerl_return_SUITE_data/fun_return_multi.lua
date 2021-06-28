function retfun(value)
    return value
end

function retfun2(value)
    return value, 2 * value
end

return retfun(7), retfun("str 1"), retfun2(5.5)
