-- File    : schema-example.lua
-- Purpose : Brief demonstration of schema.lua on Luerl.
-- See     : ./schema.erl


local s = require("schema")

local good_user = {
    id        = 12, -- id is a number
    usertype  = "admin", -- one of 'admin', 'moderator', 'user'
    nicknames = { "Nick1", "Nick2" }, -- nicknames used by this user
    rights    = { 4, 1, 7 } -- table of fixed length of types
}

local bad_user = {
    id        = 13,
    usertype  = "root",
    nicknames = { "Nick3", "Nick4" },
    rights    = { 6, 6, 6 }
}

rights = s.AllOf(s.NumberFrom(0, 7), s.Integer)

user_schema = s.Record {
    id        = s.Number,
    usertype  = s.OneOf("admin", "moderator", "user"),
    nicknames = s.Collection(s.String),
    rights    = s.Tuple(rights, rights, rights)
}

local err = s.CheckSchema(good_user, user_schema)

-- 'err' is nil if no error occured
if err then
    print(s.FormatOutput(err))
end

local err = s.CheckSchema(bad_user, user_schema)

-- 'err' is nil if no error occured
if err then
    print(s.FormatOutput(err))
end