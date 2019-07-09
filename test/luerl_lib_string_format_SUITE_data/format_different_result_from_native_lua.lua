-- source: http://lua-users.org/wiki/StringLibraryTutorial,
StringABC = "ABCDE"

return string.format("%e, %e", math.pi, math.pi)
     , string.format("%g, %g", math.pi, 10^9)
     , string.format("%d, %i, %u", -100, -100, -100)
     , ("apple"):gsub("a","A"):gsub("e","E")
     , string.format("Some different radices: %d %x %o %#x %#o", 100, 100, 100, 100, 100)
     , string.format ("%g", 15.656)
     , string.format ("%#.f", 4.4) --> 4.   -- https://www.gammon.com.au/scripts/doc.php?lua=string.format

     , string.byte("ABCDE") --> wanted=65, return with 65.0, not integer
     , string.byte("ABCDE", 2) --> wanted=66, return with 66.0, not integer
, string.byte("ABCDE",3,4)
, StringABC:byte(4)  --> direct call from variable test, wanted result: 68, integer
, string.find("Hello Lua user", "Lua")

, string.find("Hello Lua user", "e", -5)
, string.format("%#o", 15) --> in native lua: 017, in Luerl: 17

, string.format ("%x", 86543) --> 0x1520f
, string.format ("%X", 86543) --> 0X1520F
, string.format("\\ %i", "6") -- missing \ char in Luerl.
