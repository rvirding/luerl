LINENUMBER=1111
NEXTLINE=222
function never_run()
    ProoveToLaci = 0
    return -1
end
function add (X)
    PlusVal = 1; Masik=99

    -- display file/line of error in return message
    --Error = "a" + 2

    return X + PlusVal
end
AddBase = 33
if LINENUMBER < NEXTLINE then AddBase = 1 end
--never_run()
return add(AddBase-(11*3)), "string 2", 3.4
