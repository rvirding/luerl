return
      string.format("Preceding with blanks: %10d", 1977)
    , string.format("Preceding with zeros: %010d", 1977)
    , string.format ("To wield the %%s you need to be level %%i", "sword", 10)
    , string.format ("%i", 123.456) --> 123
    , string.format("%3.2i", 1)
    , string.format("%02i", "2")
    , string.format("%3i", 3)
    , string.format("%+4.2i", 4)
    , string.format("%+4.2i", -4)
    , string.format("% +5.2i", 5)
    , string.format("%d, %i", -100, -100)
    , string.format("%+d, %-d", 100, -100)
