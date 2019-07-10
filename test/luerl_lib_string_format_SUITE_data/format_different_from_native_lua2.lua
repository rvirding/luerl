return
    string.format("\\ backslash not displayed")
  , string.format("\\ %i", "6") -- missing \ char in Luerl.
  , string.format ("%x", 86543) --> 0x1520f
  , string.format ("%X", 86543) --> 0X1520F
  , string.format("%#o", 15) --> in native lua: 017, in Luerl: 17
