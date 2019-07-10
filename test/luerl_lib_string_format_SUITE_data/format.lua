-- source: http://lua-users.org/wiki/StringLibraryTutorial,
function string_and_quoted()
    return string.format("%s %q", "Hello", "Lua user!")
end

counter_ffs = 0
func_from_string = loadstring("counter_ffs = counter_ffs + 1")
func_from_string()


function detect_param_type (Param)
    if type(Param) ~= "string" then
            return "Param is not a string"
    end
    return "Param is a string"
end

return    string_and_quoted()
        , string.format("%c%c%c", 76, 117, 97)
        , string.format("%c", 999) -- char is bigger than 255
        , string.format("%f", math.pi)
        , string.format('%q', 'a string with "quotes" and \n new line')
        , string.format("Preceding with blanks: %10d", 1977)
        , string.format("Preceding with zeros: %010d", 1977)
        , string.format("%s", "parameter")
        , string.format("floats: %+f %-f", 1.23, -4.56     )
        , string.format("%.4f", "1.23456")
        , string.format("%.4s", "abcdef")
        , string.format("###%-5s###", 'x')
        , string.format ("To wield the %%s you need to be level %%i", "sword", 10)
        , string.format ("%c", 65)
        , string.format ("%i", 123.456) --> 123
        , string.format ("%x", 86543) --> 1520f
        , string.format ("%X", 86543) --> 1520F

        , string.byte("ABCDE",100)
        , string.char(65, 66, 67)
        , string.char()
        , counter_ffs
        , string.find("Hello Lua user", "banana")
        , string.find("Hello Lua user", "Lua", 8)
        , load("local a = 10; return a * 2")()
        , detect_param_type(42)
        , string.format("%02o", 12)
, string.format("%3.2i", 1)
, string.format("%02i", "2")
, string.format("%3i", 3)
, string.format("%+4.2i", 4)
, string.format("%+4.2i", -4)
, string.format("% +5.2i", 5)
, string.format("%#o", 0)
, ("apple"):gsub("a","A"):gsub("e","E")
, string.format("%d, %i", -100, -100)
