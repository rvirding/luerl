return setmetatable({
    escapes = {
        red = "\027[31m",
        green = "\027[32m",
        orange = "\027[33m",
        navy = "\027[34m",
        magenta = "\027[35m",
        cyan = "\027[36m",
        gray = "\027[90m",
        grey = "\027[90m",
        light_gray = "\027[37m",
        light_grey = "\027[37m",
        peach = "\027[91m",
        light_green = "\027[92m",
        yellow = "\027[93m",
        blue = "\027[94m",
        pink = "\027[95m",
        baby_blue = "\027[96m",

        highlight = {
            red = "\027[41m",
            green = "\027[42m",
            orange = "\027[43m",
            navy = "\027[44m",
            magenta = "\027[45m",
            cyan = "\027[46m",
            gray = "\027[47m",
            grey = "\027[47m",
            light_gray = "\027[100m",
            light_grey = "\027[100m",
            peach = "\027[101m",
            light_green = "\027[102m",
            yellow = "\027[103m",
            blue = "\027[104m",
            pink = "\027[105m",
            baby_blue = "\027[106m",
        },

        strikethrough = "\027[9m",
        underline = "\027[4m",
        bold = "\027[1m",
    },
    _sequence = '',
    _highlight = false,
    --[[ In the case that the user later overrides `print` or sets `print` to
    this table we save it for internal use
    ]]
    print = print,
    reset_global = function(self)
        _G.print = self.print
    end
},
{
    __call = function(self, ...) return self.print(...) end,

    __index = function(self, index)
        local esc = (
            self._highlight
            and rawget(self, 'escapes').highlight[index]
            or rawget(self, 'escapes')[index]
        )
        local clear = "\027[0m"
        local root_table = self
        self._highlight = index == 'highlight'
        if esc ~= nil then
            if type(esc) == 'string' then
                self._sequence = self._sequence .. esc
            end
            return setmetatable({}, {
                __call = function(proxy, ...)
                    _ = self._sequence and io.write(self._sequence)
                    root_table.print(...)
                    root_table._sequence = ''
                    io.write(clear)
                end,
                __index = function(proxy, k)
                    return root_table[k]
                end,
            })
        else
            return rawget(self, index)
        end
    end,
})

