require("table")

--[[
In this assignment you will implement self-style inheritance in Lua.

To start with you probably want to read the paper, at least up to the
setion on Dyanic Inheritance.  I will refer to the figures in the
paper at various points in the assignment.

Organizing Programs Without Classes by Ungar et. al
http://bibliography.selflanguage.org/organizing-programs.html

Getting the self-style redirection on work requires the use of lua
metatables.  I've written the code for you (though you can feel free
to modify it) but you will need to understand how it works.  Check out
these references:

http://lua-users.org/wiki/ObjectOrientationTutorial
http://lua-users.org/wiki/MetamethodsTutorial

Below is my code for implemnting forwarding members, you may modify it
if you wish.
--]]

function lookup_in_parents(table,key)
   --[[
   assumes the existing of a "parents" list on the object table if
   so, attempts to lookup missing varaibles in the parents in list
   order.  Note that this will trigger __index on the parents too,
   so if the parents have parents, they don't need to be explictly
   listed in this list.
   --]]
   for i,p in ipairs(table.parents) do
      if(p[key] ~= nil) then
         return p[key]
      end
   end
   --not found in any of the parents, so we return nil
   return nil
end

-- you might want to change this if you want your objects to have
-- custom adds or equality behaviors for example
local univeral_metatable = {__index = lookup_in_parents}

function create_raw_object()
   new_object = {}
   setmetatable(new_object,univeral_metatable)
   new_object.parents = {}
   return new_object
end


-- HERE IS AN EXAMPLE OF MY LOOKUP CODE WORKING

local example1 = create_raw_object()
-- this could also be written as "function example1.do_thing()"
example1.do_thing = function(self)
   print("doing thing")
end

example2 = create_raw_object()
example2.parents = {example1}
example2:do_thing()

--[[
STEP 0: utility functions

I include three for your convenience.  One that copys a table, another
that coverts a table to a string, and finally a string equality
assertion that I use in my unit tests.

There are utility libraries that would do this too, but I don't want
to annoy you with having to install them
--]]

-- a utility function for you that does a shallow copy but also
-- includes the metatable (does not *copy* the metatable though, which
-- might or might not be what you want)
--
-- based on version: http://lua-users.org/wiki/CopyTable
function basic_copy(table)
  local copy = {}
  for orig_key, orig_value in pairs(table) do
     copy[orig_key] = orig_value
  end
  setmetatable(copy, getmetatable(table))
  return copy
end

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

--some recursive functions that convert a table to a string
--
-- Example usage:
-- t = {['foo']='bar',11,22,33,{'a','b'}}
-- print( table.tostring( t ) )
--
-- taken from: http://lua-users.org/wiki/TableUtils
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


function assert_equals(expected, actual)
   if expected == actual then
      print("Test passed")
   else
      print("Test failed")
      print("Expected:")
      print(expected)
      print("Actual:")
      print(actual)
   end
end
--[[
STEP 1: polygon [10 points]

Create everything you need for a polygon class equivalent.

This will require at minimum:
a traits object that has a draw(self) method and a copy(self) method
a prototypical polygon that has vertices(self) and set_vertices(self,value) methods
a function make_polygon that makes a polygon based on the protype

The polygon's draw method should just return the string
"drawing {{1,2},{3,4},{5,6}}" or whatever the vertices list is
--]]


-- Your code creating traits and prototypes here

local polygon_traits = create_raw_object() -- modify this if you need to

-- Once you have everything working, implement this function
-- it should just return an ordinary (non-prototype) polygon object
function create_polygon()
   --your code here
end

local my_poly = create_polygon()
my_poly:set_vertices({{1,2},{3,4},{5,6}})
local my_poly2 = create_polygon()
my_poly2:set_vertices({{0,0},{3,4},{5,6}})
assert_equals("drawing {{1,2},{3,4},{5,6}}",my_poly:draw())
assert_equals("drawing {{0,0},{3,4},{5,6}}",my_poly2:draw())

--as a final test, let's add a new method to the traits object (the
--prototype--based inheritance equiavlent of editing the class itself).
-- all our objects should inherit this change

function polygon_traits.return_hi(self)
   return "hi"
end

assert_equals("hi",my_poly:return_hi())


--[[
STEP 2: filled_polygon [5 points]

Create the equivalent of a subclass of polygon called filled polygon

You can implement this a described in Figure 2b or Figure 3 in the
paper.

Your filled polygon should have a fill_pattern(self) getter and 
a set_fill_pattern(self,pattern_string)

The filled polygon draw method should return
drawing {{1,2},{3,4},{5,6}} FILLED WITH pattern

--]]

--Your traits and prototypes here

-- Once you have everything working, implement this function
-- it should just return an ordinary (non-prototype) polygon object
function create_filled_polygon()
   --your code here
end

local my_fill = create_filled_polygon()
my_fill:set_vertices({{1,2},{3,4},{5,6}})
my_fill:set_fill_pattern("polka dots")
assert_equals("drawing {{1,2},{3,4},{5,6}} FILLED WITH polka dots",my_fill:draw())

-- as a final test, let's add another new method to the traits 
-- object of the superclass
-- all our objects should inherit this change

function polygon_traits.return_bye(self)
   return "bye"
end

assert_equals("bye",my_fill:return_bye())


--[[
STEP 3: Dynamic inheritance [5 points]

Create two trait subclasses of polygon boxed_polygon and smooth_polygon

Then make it so that boxed poygons have a method become_smooth(self)
that makes them act like smooth polygons and smooth polygons have a
method become_boxed(self) that turns them into smooth polygons.

Implement this as described in Figure 5.

Boxed polygon's draw methods should print
   drawing {{1,2},{3,4},{5,6}} BOXED
Smooth polygon's draw methods should print
   drawing {{1,2},{3,4},{5,6}} SMOOTH

...but pretend they have a lot of methods and really different code
--]]

--Traits and prototypes here

function create_boxed_polygon()
   --Your code here
end

function create_smooth_polygon()
   --Your code here
end

local my_boxed = create_boxed_polygon()
local my_smooth = create_smooth_polygon()
my_boxed:set_vertices({{1,2},{3,4},{5,6}})
my_smooth:set_vertices({{0,0},{3,4},{5,6}})
assert_equals("drawing {{1,2},{3,4},{5,6}} BOXED",my_boxed:draw())
assert_equals("drawing {{0,0},{3,4},{5,6}} SMOOTH",my_smooth:draw())
my_boxed:become_smooth()
my_smooth:become_boxed()
assert_equals("drawing {{1,2},{3,4},{5,6}} SMOOTH",my_boxed:draw())
assert_equals("drawing {{0,0},{3,4},{5,6}} BOXED",my_smooth:draw())
