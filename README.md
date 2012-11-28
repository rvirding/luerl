Luerl - an implementation of Lua in Erlang
==========================================

An experimental implementation of Lua 5.2 written solely in pure Erlang.

Some things which are known not to be implemented or work properly:

- label and goto

- tail-call optimisation in return

- only limited standard libraries

- proper handling of __metatable

- ...

When to use Luerl
-----------------

**Fast Language Switch:** Luerl should allow you to switch between Erlang and
Lua incredibly fast, introducing a way to use very small bits of logic
programmed in Lua, inside an Erlang application, with good performance.

**Multicore:** Luerl provides a way to transparently utilize multicores. The
underlying Erlang VM takes care of the distribution.

**Microprocesses:** It should give you a Lua environment that allows you to
effortlessly run tens of thousands of Lua processes in parallel, leveraging the
famed microprocesses implementation of the Erlang VM. The empty Luerl
State footprint will be yet smaller than the C Lua State footprint.

**Forking Up:** Because of the immutable nature of the Luerl VM, it becomes
a natural operation to use the same Lua State as a starting point for multiple
parallel calculations.

However, Luerl will generally run slower than a reasonable native Lua
implementation. This is mainly due the emulation of mutable data on top of an
immutable world. There is really no way around this. An alternative would be
to implement a special Lua memory outside of the normal Erlang, but this would
defeat the purpose of Luerl. It would instead be then more logical to connect to
a native Lua.

Some valid use cases for Luerl are:

- Lua code will be run only occasionally and it wouldn't be worth
  managing an extra language implementation in the application.

- The Lua code chunks are small so the slower speed is weighed up by
  Luerl's faster interface.

- The Lua code calculates and reads variables more than changing them.

- The same Lua State is repeatedly used to 'fork up' as a basis for
  massively many parallel calculations, based on the same state.

- It is easy to run multiple instances of Luerl which could better
  utilise multicores.

There may be others.

Interface functions in luerl.erl
--------------------------------

All functions optionally accept a **Lua State** parameter. The Lua State is the state of a Lua VM instance. It can be carried from one call to the next. If no State is passed in, a new state is initiated for the function call.

Note that **Forms** (see definition below) can travel between different States. They are precompiled bits of code, independent of State. That you can 'carry them around' is unique to Luerl.

Please avoid directly accessing functions in other modules which haven't been defined here. There are no guarantees that they will not change.

**eval** and **do** functions differ only in what they return. The **do** functions return results and a new Lua State, the **eval** functions return a tuple starting on 'ok' or 'error', then the result, or cause of error.

    do --> {Result, State} 

    eval --> {ok, Result} | {error, Reason}

**Spec Definitions:**   
**Binary** means an Erlang binary string.   
**Form** means a portion of precompiled bytecode. (also ambiguously called **Compiled Chunks** here and **Chunks** in the samples.)    
**State** means a Lua State, this *is* a Lua VM instance.     
**Path** means a file system path and file name.  
**FuncPath** means an Erlang list of **atoms** representing nested names, e.g. [table,pack] for table.pack.  
**Keys** I don't know

**Eaxmples**
See below and files `hello.erl` and especially `hello2.erl` in `examples/hello/`.

#### luerl:eval(String|Binary|Form[, State]) -> {ok, Result} | {error, Reason}.
 Evaluate a Lua expression passed in as a string or binary, and return its result.

#### luerl:evalfile(Path[, State]) -> {ok, Result} | {error, Reason}.
 Load and execute a file, and return the result.

#### luerl:do(String|Binary|Form[, State]) -> {Result, NewState}.
 Evaluate a Lua expression and return its result, and the new Lua State.

#### luerl:dofile(Path[, State]) -> {Result, NewState}.
 Load and execute the Lua code in the file and return its result, and the new Lua State. Equivalent to doing luerl:eval("dofile('FileName')").

#### luerl:load(String|Binary) -> {ok, Form} | {error, Reason} .
 Parse a Lua chunk as string or binary, and return a compiled chunk ('form').

#### luerl:loadfile(Path) -> {ok,Form}.
 Parse a Lua file, and return a compiled chunk ('form').

#### luerl:init() -> State.
 Get a new Lua State = a fresh Lua VM instance.

#### luerl:call(FuncPath, Args[, State]) -> {Result,NewState}
Call a function already defined in the state. FuncPath is a list of atoms of names to the function. FuncPath, Args and Result are automatically encode/decoded.

#### luerl:call1(Keys, Args, State) -> {Result,NewState}
Call a function already defined in the state. Keys is a list of keys to the function. Keys, Args and Result are NOT encode/decoded.

#### luerl:method(FuncPath, Args[, State]) -> {Result,NewState}.
Call a method already defined in the state. FuncPath is a list of atoms of names to the function. FuncPath, Args and Result are automatically encode/decoded.

#### luerl:method1(Keys, Args, State) -> {Result,NewState}
Call a method already defined in the state. Keys is a list of keys to the function. Keys, Args and Result are NOT encode/decoded.

#### luerl:stop(State) -> GCedState.
 Garbage collects the state and (todo:) does away with it.

#### luerl:gc(State) -> State.
 Runs the (experimental) garbage collector on a state and returns the new state.
 
N.B. This interface is subject to change!

Examples

To run the examples, do `make` and then start the Erlang command line with `erl -pa ./ebin`.  
**Don't be shocked by the very long dump following each function call.**   
At the command line you are seeing the Lua State dumped, that is returned by these calls:

#### execute a string
    luerl:do("print(\"Hello, Robert(o)!\")").

#### execute a file
    luerl:dofile("./examples/hello/hello.lua").

#### separately parse, then execute
    {ok, Chunk} = luerl:load("print(\"Hello, Chunk!\")"),
    State = luerl:init(),
    {_Ret, _NewState} = luerl:do(Chunk, State).

#### call a function in the state
    {Res,State1} = luerl:call([table,pack], [<<"a">>,<<"b">>,42], State0)

executes the call `table.pack("a", "b", 42)` in `State0`. E.g.:

    {Res,State1} = luerl:call([table,pack], [<<"a">>,<<"b">>,42]),
    io:format("~p~n",[Res]).


Examples
--------

For more exhaustive examples see `examples/hello/hello2.erl`:  

`./hello.erl` is a very brief example while `examples/hello/hello2.erl` is a
comprehensive lists of most ways that come to mind of how to use the individual
 interface functions.

You can build and run these samples with:

    make examples


Library Module
--------------

There is also a library module, `luerl_lib`, which contains functions which may be used.

#### luerl_lib:first_value(ReturnList) -> Value.

#### luerl_lib:is_true_value(ReturnList) -> true | false.


Currently implemented functions in the libraries
------------------------------------------------

- _G
- _VERSION
- assert
- collectgarbage
- dofile
- eprint
- error
- getmetatable
- ipairs
- load
- loadfile
- next
- pairs
- pcall
- print
- rawequal
- rawget
- rawlen
- rawset
- select
- setmetatable
- tonumber
- tostring
- type
- io\.flush
- io\.write
- math\.abs
- math\.acos
- math\.asin
- math\.atan
- math\.atan2
- math\.ceil
- math\.cos
- math\.cosh
- math\.deg
- math\.exp
- math\.floor
- math\.fmod
- math\.frexp
- math\.huge
- math\.ldexp
- math\.log
- math\.log10
- math\.max
- math\.min
- math\.modf
- math\.pi
- math\.pow
- math\.rad
- math\.random
- math\.randomseed
- math\.sin
- math\.sinh
- math\.sqrt
- math\.tan
- math\.tanh
- os\.clock
- os\.date
- os\.difftime
- os\.getenv
- os\.time
- string\.byte
- string\.char
- string\.find
- string\.format (very limited as yet)
- string\.gmatch
- string\.gsub
- string\.len
- string\.lower
- string\.match
- string\.rep
- string\.reverse
- string\.sub
- string\.upper
- table\.concat
- table\.insert
- table\.pack
- table\.remove
- table\.sort
- table\.unpack


Known Bugs
----------

Functions defined in a loop, _while_, _repeat_ and _for_, **and** when the
loop is exited with a _break_ will generate an error when called. For
example the functions defined in

    for i=1,10 do
      a[i] = {set = function(x) i=x end, get = function () return i end}
      if i == 3 then break end
    end

**N.B.** This only occurs if the loop is actually exited with the
break, otherwise there is no problem.
