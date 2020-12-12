-- File    : chroma-example.lua
-- Purpose : Brief demonstration of ANSI colors on Luerl.
-- See     : ./chroma.erl


--[[ Here we are overriding the standard Lua print function.
    All existing print() calls will continue to work normally, though, so
    this is probably a safe override.
]]
print = require("chroma")

--Let's print a scary greeting
print.red("hello, world!")

--Perhaps that wasn't aggressive enough?
print.red.underline.bold("I said 'Hello, World!...'\n\n")
--Maybe that was too much; perhaps a cooler tone?
print.blue.bold.highlight.navy("Oh, I'm afraid I just blue myself")

--[[ We can also do that in a different order because the formatting
        table indexes are generated dynamically
]]
print.bold.green("Such a poor choice of words\n\n")
--Let's print today's headline

print.highlight.gray(string.rep(' ', 80))
print.yellow.bold.underline.highlight.navy("Do you know the muffin man?")
print.highlight.gray(string.rep(' ', 80)..'\n\n')

--If we want to just use print as normal, then why not?
print('"Normal Printing never seemed so possible"')