
Module System

module
import nothing
export x: Int
begin
    x : Int = 3
end.

module
import x: Int
export f:Int -> Int, z: Int
begin
    f : Int -> Int  = \y: Int. y + x
    z : Int = f(x)
end.

module IO





<!-- Bindings -->
Let x = 10

or

Define x

x = 10