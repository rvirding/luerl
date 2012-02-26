-------------------------------------------------------------------------------
--- Package     : Luabench - ASCII plotter for Lua performance over i       ---
--- File        : luabench.lua                                              ---
--- Description : Main and only module file                                 ---
--- Version     : 0.1.0 / alpha                                             ---
--- Copyright   : 2011 Henning Diedrich, Eonblast Corporation               ---
--- Author      : H. Diedrich <hd2010@eonblast.com>                         ---
--- License     : see file LICENSE                                          ---
--- Created     : 07 Apr 2011                                               ---
--- Changed     : 08 Apr 2011                                               ---
-------------------------------------------------------------------------------
---                                                                         ---
---  ASCII plotted graph showing performance relative to element count.     ---
---                                                                         ---
---  Use: luabench=require("luabench")                                      ---
---  See: sample.lua                                                        ---
---                                                                         ---
-------------------------------------------------------------------------------

module("luabench", package.seeall)

-----------------------------------------------------------------------
-- settings: defaults
-----------------------------------------------------------------------

-- change these values in your main script, see samples

TITLE = "-=xXx=- Luabench"
NAME1 = ""                    -- name to + interval bracket right of plot
NAME2 = ""                    -- name to x interval bracket right of plot
SYMBOL1 = '+'
SYMBOL2 = 'x'
SYMBOL_OVERLAP = '*'
BACKGROUND = ' '              

MAX_HEIGHT   = 30             -- max height of graph area in terminal lines
WIDTH        = 50             -- width of graph are in terminal columns
MAX_CYCLES   = 10000          -- max number of times the test functions run 
MAX_ELEMENTS = 1000           -- upper limit of number of elements (x)
BESTOF       = 3              -- number of repeat runs, fastest is used
VERBOSITY    = 1              -- debugging: 0 = quiet, 1, 2 = verbose
SHOW_ONE     = false          -- setting false often increases resolution
CUTOFF       = true           -- don't show the area from y0 to ymin

MARGIN = "    "
PLOTTER_BANNER = "-=xXx=- Luabench Plotter 0.1.0 - http://eonblast.com/luabench"

-- max y is calculated automatically. 
-- min elements is fix 1

-----------------------------------------------------------------------
-- output formatting and math (except graph)
-----------------------------------------------------------------------

sep =   "---------------------------------------" ..
        "-------------------------------------o-"
subsep= "......................................." ..
        "......................................."

-- percent value with no decimals >= 2, but one decimal < 2. ----------
function prcstr(part, base)
    if secnd == 0 then return 0 end
    x = math.floor(part / base * 100)
    if(x <= 2) then
        x = math.floor(part / base * 1000) / 10
    end
    return x
end

printcol = 0
function printf(...)
	s = string.format(...)
	if(s:find("\n")) then printcol = 0 else printcol = printcol + s:len() end
    io.write(s)
end

-- verbosity controlled conditional printing --------------------------
function printfv(vl,...)
    if vl <= VERBOSITY then printf(...) end
end

function tab(x)
	while(printcol < x) do io.write(" "); printcol = printcol + 1 end
end

function tabv(vl,x)
    if vl <= VERBOSITY then tab(x) end
end

waitdots_count = 0
function waitdots()
    if waitdots_count == 0 then margin() end
    io.write('.'); io.flush() 
    waitdots_count = waitdots_count + 1
end

function delete_waitdots()
    for i = 1,waitdots_count do
        io.write("\b"); io.flush()
    end
    -- io.write("\b\b\b\b\b\b\b\b\b\b\b\b\b")
end

function nanosecs_per(time, per)
	return time * 1000000000 / per
end

function microsecs_per(time, per)
	return time * 1000000 / per
end

function margin(vl)
    if vl == nil or vl <= VERBOSITY then io.write(MARGIN) end
end

-- decimal point ------------------------------------------------------
-- by Richard Warburton http://lua-users.org/wiki/FormattingNumbers ---
function comma_value(n)
	local left,num,right = string.match(n,'^([^%d]*%d)(%d*)(.-)$')
	return left..(num:reverse():gsub('(%d%d%d)','%1,'):reverse())..right
end

-----------------------------------------------------------------------
-- mini json stringify for output
-----------------------------------------------------------------------
-- from http://lua-users.org/wiki/TableUtils

function table.val_to_str ( v )
  if "string" == type( v ) then
    v = string.gsub( v, "\n", "\\n" )
    if string.match( string.gsub(v,"[^'\"]",""), '^"+$' ) then
      return "'" .. v .. "'"
    end
    return '"' .. string.gsub(v,'"', '\\"' ) .. '"'
  else
    return "table" == type( v ) and table.tostring( v ) or
      tostring( v )
  end
end

function table.key_to_str ( k )
  if "string" == type( k ) and string.match( k, "^[_%a][_%a%d]*$" ) then
    return k
  else
    return "[" .. table.val_to_str( k ) .. "]"
  end
end

function table.tostring( tbl )
  local result, done = {}, {}
  for k, v in ipairs( tbl ) do
    table.insert( result, table.val_to_str( v ) )
    done[ k ] = true
  end
  for k, v in pairs( tbl ) do
    if not done[ k ] then
      table.insert( result,
        table.key_to_str( k ) .. "=" .. table.val_to_str( v ) )
    end
  end
  return "{" .. table.concat( result, "," ) .. "}"
end


-----------------------------------------------------------------------
-- Terminal plotter
-----------------------------------------------------------------------

function tlimits(t)
    if #t == 0 then return nil,nil end
    min = nil 
    max = nil
    for _,v in next, t do 
        if min == nil then min = v
        elseif v ~= nil then min = math.min(min,v)
        end
        if max == nil then max = v
        elseif v ~= nil then max = math.max(max,v)
        end
    end
    return min,max
end
function plot_graph2(title, p1, p2, name1, NAME1)

    -- take #1 out -- often gains much resolution for the graph
    if not SHOW_ONE then
        suppressed = "x=1 "..SYMBOL1..": " .. t2[1] .. "; "..SYMBOL2..": " .. t3[1]
        t2[1] = nil
        t3[1] = nil
    end
    
    imax = math.max(#p1,#p2)
    vmin1,vmax1 = tlimits(p1)
    vmin2,vmax2 = tlimits(p2)
    
    if vmax1 == nil and vmax2 == nil then
        print("Can't plot ", vmin1, vmax2, vmin2, vmin2)
        return
    end

    if vmin1 == nil then vmin1 = 0 end
    if vmax1 == nil then vmax1 = 0 end
    if vmin2 == nil then vmin2 = 0 end
    if vmax2 == nil then vmax2 = 0 end

    vmin = math.min(vmin1,vmin2)
    vmax = math.max(vmax1,vmax2)
    bar  = 10 ^ (math.ceil(math.log10(vmax / 100))) 
    ymax = math.max(10,math.ceil( vmax / bar ) * bar)
    ymin = math.max( 0,math.ceil( vmin / bar - 1) * bar)
    if not CUTOFF then ymin = 0 end
    yspan = ymax - ymin
    
    step = math.ceil(yspan / MAX_HEIGHT / 10) * 10

    if step == 10 then
        altstep = math.ceil(yspan / MAX_HEIGHT / 5) * 5
        if yspan / altstep < MAX_HEIGHT then
            step = altstep
        end
    end

    if step == 5 then
        altstep = math.ceil(yspan / MAX_HEIGHT / 2) * 2
        if yspan / altstep < MAX_HEIGHT then
            step = altstep
        end
    end

    if step == 2 then
        altstep = math.ceil(yspan / MAX_HEIGHT)
        if yspan / altstep < MAX_HEIGHT then
            step = altstep
        end
    end

    -- start output    
    margin()
    print("nsec/element  " .. title)
    
    -- print(#p1, #p2, vmin, vmax, MAX_HEIGHT, step)
    
    ylast = ymax * step  -- guaranteed over

    for y = ymax,ymin,-step do
    
        plotrow2(y, ylast, p1, p2, name1, NAME1)

        ylast = y

    end

    -- cut off, dotted x axis
    if CUTOFF and ylast > 0 then
        margin()
        io.write(string.format("%10s |", "..."))
        for x = 1,imax,1 do io.write(".") end
        print()
    -- zero level x axis
    elseif ylast ~= 0 then
        plotrow2(0, ylast, p1, p2, name1, NAME1)
    end

    -- x axis legend
    ----------------
    xl = {}
    for items  = 0,MAX_ELEMENTS,math.ceil(MAX_ELEMENTS / WIDTH) do
        if items == 0 then items = 1 end
    -- clone of this loop head at (*) !!
        xl[#xl+1] = items
    end

    margin()
    io.write(string.format("%10.10s  ", "elements:"))

    for x = 1,imax,10 do
        leg = "^" .. xl[x]
        io.write(leg)
        for li = 1,10-leg:len()-1 do io.write(' ') end
    end
    print()

    if not SHOW_ONE then
        margin()
        print(suppressed)
    end 
end


function plot_graph1(title, p1, name1)

    -- take #1 out -- often gains much resolution for the graph
    if not SHOW_ONE then
        suppressed = " x=1: " .. t2[1] .. " not shown"
        t2[1] = nil
    end
    
    imax = #p1
    vmin1,vmax1 = tlimits(p1)
    
    if vmax1 == nil then
        print("Can't plot ", vmin1, vmax1)
        return
    end

    if vmin1 == nil then vmin1 = 0 end
    if vmax1 == nil then vmax1 = 0 end

    vmin = vmin1
    vmax = vmax1
    bar  = 10 ^ (math.ceil(math.log10(vmax / 100))) 
    ymax = math.max(10,math.ceil( vmax / bar ) * bar)
    ymin = math.max( 0,math.ceil( vmin / bar - 1) * bar)
    if not CUTOFF then ymin = 0 end
    yspan = ymax - ymin
    
    step = math.ceil(yspan / MAX_HEIGHT / 10) * 10

    if step == 10 then
        altstep = math.ceil(yspan / MAX_HEIGHT / 5) * 5
        if yspan / altstep < MAX_HEIGHT then
            step = altstep
        end
    end

    if step == 5 then
        altstep = math.ceil(yspan / MAX_HEIGHT / 2) * 2
        if yspan / altstep < MAX_HEIGHT then
            step = altstep
        end
    end

    if step == 2 then
        altstep = math.ceil(yspan / MAX_HEIGHT)
        if yspan / altstep < MAX_HEIGHT then
            step = altstep
        end
    end

    -- start output    
    margin()
    print("nsec/element  " .. title)
    
    -- print(#p1, #p2, vmin, vmax, MAX_HEIGHT, step)
    
    ylast = ymax * step  -- guaranteed over

    for y = ymax,ymin,-step do
    
        plotrow1(y, ylast, p1, name1)

        ylast = y

    end

    -- cut off, dotted x axis
    if CUTOFF and ylast > 0 then
        margin()
        io.write(string.format("%10s |", "..."))
        for x = 1,imax,1 do io.write(".") end
        print()
    -- zero level x axis
    elseif ylast ~= 0 then
        plotrow1(0, ylast, p1, name1)
    end

    -- x axis legend
    ----------------
    xl = {}
    for items  = 0,MAX_ELEMENTS,math.ceil(MAX_ELEMENTS / WIDTH) do
        if items == 0 then items = 1 end
    -- clone of this loop head at (*) !!
        xl[#xl+1] = items
    end

    margin()
    io.write(string.format("%10.10s  ", "elements:"))

    for x = 1,imax,10 do
        leg = "^" .. xl[x]
        io.write(leg)
        for li = 1,10-leg:len()-1 do io.write(' ') end
    end
    print()

    if not SHOW_ONE then
        margin()
        print(suppressed)
    end 
end

function plotrow1(y, ylast, p1, name1)

        margin()        
        io.write(string.format("%10d |", y))
        local s = {}
        local u1

        for x = 1,imax,1 do
            local y1 = p1[x]

            -- x=1 taken out
            if not SHOW_ONE and x == 1 then
                io.write(':')
            else
                -- hit?
                if y1 == nil then y1 = 0 end
                if y1 ~= nil and y1 >= y and y1 < ylast then 
                    u1 = true
                else
                    u1 = false
                end
                
                -- plot
                if u1 then io.write(SYMBOL1)
                elseif y == 0 then
                    if y1==nil then io.write(':')
                    else io.write('-')
                    end
                else 
                    io.write(BACKGROUND)
                end
            end
        end
        
        if u1 then 
            min,max = tlimits(p1)
            io.write(string.format(" [%d..%d] %s  ", min, max, name1))
        end

        print()
end

function plotrow2(y, ylast, p1, p2, name1, name2)

        margin()        
        io.write(string.format("%10d |", y))
        local s = {}
        local u1, u2

        for x = 1,imax,1 do
            local y1 = p1[x]
            local y2 = p2[x]

            -- x=1 taken out
            if not SHOW_ONE and x == 1 then
                io.write(':')
            else
                -- hit?
                if y1 == nil then y1 = 0 end
                if y2 == nil then y2 = 0 end
                if y1 ~= nil and y1 >= y and y1 < ylast then 
                    u1 = true
                else
                    u1 = false
                end
                if y2 ~= nil and y2 >= y and y2 < ylast then 
                    u2 = true
                else
                    u2 = false
                end
                
                -- plot
                if u1 == true and u2 == true then io.write(SYMBOL_OVERLAP)
                elseif u1 then io.write(SYMBOL1)
                elseif u2 then io.write(SYMBOL2)
                elseif y == 0 then
                    if y1==nil or y2==nil then io.write(':')
                    else io.write('-')
                    end
                else 
                    io.write(BACKGROUND)
                end
            end
        end
        
        if u1 then 
            min,max = tlimits(p1)
            if u2 then io.write("  "..SYMBOL1..": ") end
            io.write(string.format(" [%d..%d] %s  ", min, max, name1))
        end

        if u2 then 
            if u1 then io.write("  "..SYMBOL2..": ") end
            min,max = tlimits(p2)
            io.write(string.format(" [%d..%d] %s", min, max, name2))
        end

        print()
end

-----------------------------------------------------------------------
-- random contents creation
-----------------------------------------------------------------------

local abc = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}

function randstr(max)
      local r = {}
      local length = math.random(1,max)
      local i
      for i = 1, length do
         r[i] = abc[math.random(1, 26)]
      end
      return table.concat(r)
end

function someval()
    y = math.random(2,10)
    if y == 1 then return nil end
    if y == 2 then return false end 
    if y == 3 then return true end 
    if y == 4 then return math.random(1,10000000) end 
    if y == 5 then return math.random(-10000000,-10000000) end 
    if y == 6 then return math.random(-1000000000,-1000000000) / 1000000 end 
    if y == 7 then return abc[math.random(1,26)] end 
    if y == 8 then return randstr(5) end 
    if y == 9 then return randstr(15) end 
    if y == 10 then return randstr(100) end 
end


function stringify(t) table.val_to_str( t ) end

-----------------------------------------------------------------------
-- measure one formula, CYCLE times, including test data preparation
-----------------------------------------------------------------------

local t = {}
local function measure(items, prepP, prepare, actionP, action, printPrepP)

  local cycles = math.max(1,math.ceil(MAX_CYCLES / items))
  local clock  = os.clock

  printfv(2, "%dx %-12s ", cycles, actionP)

  if prepare ~= nil then
    t = prepare(i)
    print("prep done")
  end

  local last = nil
  local best = nil

  for k = 1,BESTOF do
      local tm = clock()
      for i = 1, cycles do last = action(t) end
      if best == nil then
          best = clock() - tm
      else
          best = math.min(best, clock() - tm)
      end
  end  
  tm = best
  
  --if DYN and tm == 0 then MAX_CYCLES = math.ceil(MAX_CYCLES * 4) end 
  --if DYN and tm > 1 then MAX_CYCLES = math.ceil(MAX_CYCLES / 2) end 

  if tm ~= 0 then
  	 mspc= nanosecs_per(tm, cycles * items)
  	 tabv(2,27)
  	 printfv(2, "%10.0fns/element ", mspc)
  else
	  mspc = nil
	  printfv(2, "%dx %-12s ** sample too small, could not measure, increase MAX_CYCLES ** ", cycles, actionP)
  end
  
  return mspc, last 

end

-----------------------------------------------------------------------
-- main function
-----------------------------------------------------------------------

function plot1(title, prepP, prepare, prompt1, action1)

    margin()
    if(_PATCH) then io.write(_PATCH) else io.write(_VERSION .. ' official') end
    print()

    margin()
    print(sep)
    margin()
    printf("x=[%d..%d] elements in a table %-25s\n", 1, MAX_ELEMENTS, prepP)
    margin()
    print("y=time/x needed for " .. prompt1)
    margin()
    print(sep)

    collectgarbage()

    t2 = {}
    for items  = 1,MAX_ELEMENTS,math.ceil(MAX_ELEMENTS / WIDTH) do
        if items == 0 then items = 1 end
    -- clone of this loop head at (*) !
        
        if(VERBOSITY < 2) then waitdots() end
        
        t = prepare(items)

        if anyprev then margin(2); printfv(2, subsep .. "\n") end
        margin(2)
        printfv(2, "%d elements in %-25s\n", items, prepP)
        margin(2)
        printfv(2, subsep .. "\n")

        secnd, r2 = measure(items, prepP, nil, prompt1, action1, true)
        if secnd == nil then secnd = 0 end
        t2[#t2+1] = secnd        
        printfv(2, "     %.20s.. \n", r2)

        margin(2)
        printfv(2, "     %.20s.. \n", r3)
       
        anyprev = true
    end
    
    if(VERBOSITY < 2) then delete_waitdots() end
    
    plot_graph1(title, t2, NAME1)

    margin()
    print(PLOTTER_BANNER)
    
end

function plot2(title, prepP, prepare, prompt1, action1, prompt2, action2)

    margin()
    if(_PATCH) then io.write(_PATCH) else io.write(_VERSION .. ' official') end
    print()

    margin()
    print(sep)
    margin()
    printf("%d - %d elements in %-25s\n", 1, MAX_ELEMENTS, prepP)
    margin()
    print("+: " .. prompt1)
    margin()
    print("x: " .. prompt2)
    margin()
    print(sep)

    collectgarbage()

    t2 = {}; t3 = {}
    for items  = 1,MAX_ELEMENTS,math.ceil(MAX_ELEMENTS / WIDTH) do
        if items == 0 then items = 1 end
    -- clone of this loop head at (*) !
        
        if(VERBOSITY < 2) then waitdots() end
        
        t = prepare(items)

        if anyprev then margin(2); printfv(2, subsep .. "\n") end
        margin(2)
        printfv(2, "%d elements in %-25s\n", items, prepP)
        margin(2)
        printfv(2, subsep .. "\n")

        secnd, r2 = measure(items, prepP, nil, prompt1, action1, true)
        if secnd == nil then secnd = 0 end
        t2[#t2+1] = secnd        
        printfv(2, "     %.20s.. \n", r2)

        third, r3 = measure(items, prepP, nil, prompt2, action2)
        if third == nil then third = 0 end
        t3[#t3+1] = third
        
        if(secnd and third) then prc = prcstr(third, secnd)
            margin(2)
            printfv(2, "%3g%% %.20s.. \n", prc, r3)
        else 
            prc = "-"
            margin(2)
            printfv(2, "     %.20s.. \n", r3)
        end
       
        anyprev = true
    end
    
    if(VERBOSITY < 2) then delete_waitdots() end
    
    plot_graph2(title, t2, t3, NAME1, NAME2)

    margin()
    print(PLOTTER_BANNER)
    
end

function plot(title, prepP, prepare, prompt1, action1, prompt2, action2)

    if prompt2 == nil or action2 == nil then
        plot1(title, prepP, prepare, prompt1, action1)
    else
        plot2(title, prepP, prepare, prompt1, action1, prompt2, action2)
    end
end