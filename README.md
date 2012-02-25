Luerl - an implementation of Lua in Erlang
------------------------------------------

An experimental implementation of Lua 5.2 written solely in pure Erlang.

Some things which are known not to be implemented or work properly:

- label and goto

- tail-call optimisation in return

- only limited standard libraries

- proper handling of __metatable

- ...


These are the interface functions in luerl.erl:

All functions optionally accept a **Lua State** parameter. The Lua State is the state of a Lua VM instance. It can be carried from one call to the next. If no State is passed in, a new state is initiated for the function call.

**eval** and **do** functions differ only in what they return. The **do** functions return results and a new Lua State, the **eval** functions return a tuple starting on 'ok' or 'error', then the result, or cause of error.

    do --> {Result, State} 

    eval --> {ok, Result} | {error, Reason}

The 'compile' functions double the 'load' function, with but a different name.

#### luerl:eval(String|Binary|Form[, State]) -> {ok, Result} | {error, Reason}.
 Evaluate a Lua expression passed in as a string or binary, and return its result.

#### luerl:evalfile(Path[, State]) -> {ok, Result} | {error, Reason}.
 Load and execute a file, and return the result.

#### luerl:do(String|Binary|Form[, State]) -> {Result, NewState}.
 Evaluate a Lua expression and return its result, and the new Lua State.

#### luerl:dofile(Path[, State]) -> {Result, NewState}.
 Load and execute the Lua code in the file and return its result, and the new Lua State. Equivalent to doing luerl:eval("dofile('FileName')").

#### load(String|Binary) -> {ok, Form} | {error, Reason} .
 Parse a Lua chunk as string or binary, and return a compiled chunk.

#### loadfile(Path) -> {ok,Form}.
 Parse a Lua file, and return a compiled chunk.

#### compile(String|Binary) -> {ok, Form} | {error, Reason} .
 Parse a Lua chunk as string or binary, and return a compiled chunk.
 Same as load/1.

#### compilefile(Path) -> {ok,Form}.
 Parse a Lua file, and return a compiled chunk.
 Same as loadfile/1.

#### start() -> State.
 Get a new Lua State = a fresh Lua VM instance.

#### call(Form[, State]) -> {Result,State}
 Execute a compiled chunk.

#### stop(State) -> GCedState.
 Garbage collects the state and (todo:) does away with it.

#### gc(State) -> State.
 Runs the (experimental) garbage collector on a state and returns the new state.

N.B. This interface is subject to change!


Examples

#### execute a string
    luerl:do("print(\"Hello, Robert(o)!\")"),

#### execute a file
    luerl:dofile("./examples/hello/hello.lua"),

#### separately parse, then execute
    {ok, Chunk} = luerl:load("print(\"Hello, Chunk!\")"),
    State = luerl:start(),
    {_Ret, _NewState} = luerl:do(Chunk, State),


For more examples see `examples/hello/hello2.erl`.

`./hello.erl` is a very brief example while `examples/hello/hello2.erl` is a comprehensive lists of most ways that come to mind of how to use the individual interface functions.

You can build and run these samples with:

    make hello
    make hello2



Currently implemented functions in the libraries:

_G
_VERSION
assert
collectgarbage
dofile
eprint
error
getmetatable
ipairs
next
pairs
print
rawequal
rawget
rawlen
rawset
select
setmetatable
tonumber
tostring
type

math.abs
math.acos
math.asin
math.atan
math.atan2
math.ceil
math.cos
math.cosh
math.deg
math.exp
math.floor
math.log
math.max
math.min
math.pi
math.pow
math.rad
math.sin
math.sinh
math.sqrt
math.tan
math.tanh

os.difftime
os.getenv
os.time

string.byte
string.char
string.len
string.lower
string.rep
string.reverse
string.upper

table.concat
table.pack
table.unpack
