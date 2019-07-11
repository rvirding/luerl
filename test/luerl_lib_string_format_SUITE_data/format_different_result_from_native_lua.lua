-- source: http://lua-users.org/wiki/StringLibraryTutorial,
StringABC = "ABCDE"

return
      string.format("%e, %.4e", math.pi, math.pi)
    , string.format("%g, %.3g", math.pi, math.pi)

    , string.format("Some different radices: %d %x %o %#x %#o", 100, 100, 100, 100, 100)
    , string.format ("%g", 15.656)
    , string.format ("%G", 15.656)
    , string.format ("%#.F", 3.3) --> It doesn't work in native lua.
    , string.format ("%#.f", 4.4) --> 4.   -- https://www.gammon.com.au/scripts/doc.php?lua=string.format
    , string.format ("%#.E", 5.5) --> 6.E+00   -- https://www.gammon.com.au/scripts/doc.php?lua=string.format

    , string.byte("ABCDE") --> wanted=65, return with 65.0, not integer
    , string.byte("ABCDE", 2) --> wanted=66, return with 66.0, not integer
    , string.byte("ABCDE",3,4)
    , StringABC:byte(4)  --> direct call from variable test, wanted result: 68, integer

