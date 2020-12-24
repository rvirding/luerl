-- File    : utf8-example.lua
-- Purpose : Brief demonstration of utf8.lua on Luerl.
-- See     : ./utf8.erl


local utf8 = require("utf8char")

-- Provides UTF-8 aware string functions implemented in pure lua:

-- * utf8.len(s)
-- * utf8.sub(s, i, j)
-- * utf8.reverse(s)
-- * utf8.char(unicode)
-- * utf8.unicode(s, i, j)
-- * utf8.gensub(s, sub_len)
-- * utf8.find(str, regex, init, plain)
-- * utf8.match(str, regex, init)
-- * utf8.gmatch(str, regex, all)
-- * utf8.gsub(str, regex, repl, limit)

-- All functions behave as their non UTF-8 aware counterparts with the exception
-- that UTF-8 characters are used instead of bytes for all units.

print( utf8.len("bonjour le monde, c'est la lune qui parle") )
print( utf8.len("이 문자열은 한국어입니다") )
print( utf8.len("Эта строка в кириллице") )
print( utf8.len("這個字符串的中國") )
print( utf8.len("هذه السلسلة باللغة العربية") )
print( utf8.len("דאס שטריקל איז אין ייִדיש") )
print( utf8.len("Овај низ подржава окружење") )
print( utf8.len("สวัสดีชาวโลกนี่คือพระจันทร์พูด") )
print( utf8.len("Hello world, これは月の話です") )

print( utf8.sub("스타크래프트 인공지능", 5, 8) )

print( utf8.reverse("스타크래프트 인공지능") )

-- KLua-String: A Korean language support library for Lua
-- https://github.com/airtaxi/KLua-String

local klua = require("klua")

-- KLua provides Korean-related string functions that are not supported by Lua.
-- Basic functions such as find and replace are based on utf8, and you can use
-- the same utf8.lua function names after loading KLua-String.

klua.merge({"ㅁ", "ㅣ", "ㅋ"}) -- 밐

klua.split("스타크래프트 인공지능!!")

print( klua.exists("미쿠의 머리카락은 정말 길어서 바닥에 쓸릴 것 같아", "ㅎ") )  -- false
print( klua.exists("미쿠의 머리카락은 정말 길어서 바닥에 쓸릴 것 같아", "ㅋ") )  -- true
print( klua.exists("미쿠의 머리카락은 정말 길어서 바닥에 쓸릴 것 같아", "미쿠") ) -- true
print( klua.exists("Live for the Swarm", "S") ) -- true

-- utf8.lua function names can be used after loading KLua-String.

print( klua.len("스타크래프트 인공지능") )