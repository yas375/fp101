import Data.Char

n = a `div` length xs
    where
      a = 10
      xs = [1..5]

-- return last element of the list
last1 ns = head (drop (length ns - 1) ns)
last2 ns = ns !! (length ns - 1)
last3[x] = x
last3(x:xs) = last3(xs)

-- remove last element of the list
init1 ns = take (length ns - 1) ns
init2 ns = reverse (drop 1 (reverse ns))
init3[x] = []
init3(x:xs) = [x] ++ init3 xs


sum2[] = 10
sum2(x:xs) = x + sum2 xs


qsort2[] = []
{-qsort2 (x : xs) = qsort2 smaller ++ [x] ++ qsort2 larger-}
qsort2 (x : xs) = reverse(reverse(qsort2 smaller) ++ [x] ++ reverse(qsort2 larger))
                  where
                    smaller = [a | a <- xs, a <= x ]
                    larger = [ b | b <- xs, b > x ]

-- Define a function product that produces the product
-- of a list of numbers, and show using your definition
-- that product [ 2, 3, 4 ] = 24.
-- product2[x] = x
product2[] = 1
product2(x:xs) = x * product2 xs

-- How should the definition of the function qsort be
-- modified so that it produces a reverse sorted version
-- of a list?
qsort_r[] = []
qsort_r(x : xs) = qsort_r larger ++ [x] ++ qsort_r smaller
                  where
                    smaller = [a | a <- xs, a <= x ]
                    larger = [ b | b <- xs, b > x ]

{-
3,5,4,1,2

      3
1,2       5,4


1,2

    1
        2
2,1


1,2   3



5,4

     5
4


5,4


1,2,3,5,4



-}



mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z


second xs = head (tail xs) -- :: [a] -> a
swap (x, y) = (y,x) -- :: (a, b) -> (b, a)
pair x y = (x, y) -- :: a -> b -> (a, b)
double x = x * 2 -- Num a => a -> a
palindrome xs = reverse xs == xs -- Eq a [a] -> Bool
twise f x = f (f x) -- a -> b -> a


-- Chapter 4. Defining functions

-- 1. Using library functions, define a function halve :: [ a ] → ([ a ], [ a ]) that splits an even-lengthed list into two halves.

halve     :: [a] -> ([a], [a])
halve a | length a `mod` 2 == 0   = (take h a, drop h a) where h = length a `div` 2


{-halve [a] = (take halve a, drop )-}

-- 2. Consider a function safetail :: [ a ] → [ a ] that behaves as the library
-- function tail, except that safetail maps the empty list to itself, whereas
-- tail produces an error in this case. Define safetail using:
-- (a) a conditional expression;

safetail_a a = if null a then a else tail a

-- (b) guarded equations;

safetail_b a | null a     = a
             | otherwise  = tail a

-- (c) pattern matching.

safetail_c [] = []
safetail_c (_ :xs) = xs


safetail_d xs
  | null xs = []
  | otherwise = tail xs


safetail_e [] = []
safetail_e xs = tail xs

safetail_f
  = \ xs ->
      case xs of
        [] -> []
        (_ : xs) -> xs

-- 3. In a similar way to ∧, show how the logical disjunction operator ∨ can be
-- defined in four different ways using pattern matching.

(|#|)           :: Bool -> Bool -> Bool
True |#| True   =  True
True |#| False  =  True
False |#| True  =  True
False |#| False =  False

False |%| False = False
_ |%| _ = True


False |^| b = b
_ |^| _ = True

b ||| False = b
_ ||| True = True


b |*| c | b == c && b == False   = False
        | otherwise              = True


(|$|) b c = if b == c then b else True

-- 4. Redefine the following version of the conjunction operator using conditional
-- expressions rather than pattern matching:
-- True ∧ True = True
-- _ ∧ _ = False
(&^&) a b = if a == b && a == True then True else False
(&^^&) a b = if a == b then a else False

-- 5. Do the same for the following version, and note the difference in the number
-- of conditional expressions required:
-- True  ∧ b = b
-- False ∧ _ = False

(&$&) a b = if a then b else False

-- 6. Show how the curried function definition mult x y z = x * y * z can be understood
-- in terms of lambda expressions.

mult1 = \x -> (\y -> (\z -> x * y * z))

a &@& b = if not (a) then not (b) else True

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

e3 x = x * 2
e9 [x, y] = (x, True)
e10 (x, y) = [x, y]

e13 x y = x + y * y


-- Chapter 5. List comprehensions

-- 1.  Using a list comprehension, give an expression that calculates the
-- sum 1^2 + 2^2 + . . . 100^2 of the first one hundred integer squares.
  -- sum [x^2 | x <- [1..100]]

xs123 = 1 : [x + 1 | x <- xs123]


pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]


-- [(x, y) | x <- [1..3], y <- [4..6]]
-- 1,4  1,5  1,6  2,4  2,5  2,6  3,4  3,5  3,6

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x,y) <- zip xs ys]

divides :: Int -> Int -> Bool
divides a b = a `mod` b == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]

find :: (Eq a) => a -> [(a, b)] -> [b]
find k ts = [v | (k2, v) <- ts, k == k2]

positions :: (Eq a) => a -> [a] -> [Int]
positions a xs = find a (zip xs [0..n])
                 where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
-- scalarproduct xs ys = sum (zipWith (*) xs ys)


-- Ceasar cipher

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count c xs = length [x | x <- xs, x == c]

{-
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
-}

table :: [ Float ]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral(n) / fromIntegral(m)) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs


-- Modify the Caesar cipher program to also handle upper-case letters
{-
--
let2int :: Char -> Int
let2int c | isUpper c = ord c - ord 'A'
          | otherwise = ord c - ord 'a' + 26

int2char :: Int -> Char
int2char n | n < 26    = chr (ord 'A' + n)
           | otherwise = chr (ord 'a' + n - 26)

shift :: Int -> Char -> Char
shift n c | isLetter c = int2char((let2int c + n) `mod` 52)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
-}


let2int :: Char -> Int
let2int c | isUpper c = ord c - ord 'A'
          | otherwise = ord c - ord 'a' + 26

int2char :: Int -> Char
int2char n | n < 26    = chr (ord 'A' + n)
           | otherwise = chr (ord 'a' + n - 26)

shift :: Int -> Char -> Char
shift n c | isUpper c = int2char((let2int c + n) `mod` 26)
          | isLower c = int2char((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- Recursive functions

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n-1) x

(!#!) :: [a] -> Int -> a
(!#!) (x:_) 0 = x
(!#!) (_:xs) n = (!#!) xs (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs) | x == a         = True
               | otherwise      = elem1 a xs

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) | x == False = False
            | otherwise = and1 xs

and2 [] = True
and2 (b:bs) = b && and2 bs


and3 [] = True
and3 (b:bs)
  | b = and3 bs
  | otherwise = False

and4 [] = True
and4 (b:bs) = and bs && b

and5 [] = True
and5 (b:bs)
  | b = b
  | otherwise = and5 bs


concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (xs:xss) = xs ++ concat1 xss



merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] yx = yx
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys



halve2 :: [a] -> ([a], [a])
halve2 xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve2 xs


msort1 :: Ord a => [a] -> [a]
msort1 [] = []
msort1 [x] = [x]
msort1 xs = msort1 (msort1 ys ++ msort1 zs)
  where (ys, zs) = halve2 xs


