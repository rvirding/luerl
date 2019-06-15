-- source: http://lua-users.org/wiki/StringLibraryTutorial,
return string.format("%e, %e", math.pi, math.pi)
     , string.format("%g, %g", math.pi, 10^9)
     , string.format("%d, %i, %u", -100, -100, -100)
     , ("apple"):gsub("a","A"):gsub("e","E")
     , string.format("Some different radices: %d %x %o %#x %#o", 100, 100, 100, 100, 100)
     , string.format ("%g", 15.656)
     , string.format ("%#.f", 4.4) --> 4.   -- https://www.gammon.com.au/scripts/doc.php?lua=string.format
