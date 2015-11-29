-- Here are two similar implementations of the function `and`.

and1 [] = True
and1 (b:bs) = b && and1 bs

-- And another variant:

and2 [] = True
and2 (b:bs) = and2 bs && b

-- On small input lists they will produce same results. But if we have a very big list then one of them might fail because of stack overflow.

a = False : [True | _ <- [1..]]
--    and1 a
--    and2 a


-- Shouldn't one of the implementations be considered as incorrect?

-- http://screencast.com/t/24P3zpUrwV
