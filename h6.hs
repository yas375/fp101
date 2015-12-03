asd f p xs = [f x | x <- xs, p x]

a1 f p xs = filter p (map f xs)
a2 f p xs = map f (filter p xs)


all1 :: (a -> Bool) -> [a] -> Bool
all1 p xs = and (map p xs)

all2 :: (a -> Bool) -> [a] -> Bool
all2 p = not . any (not . p)

all3 :: (a -> Bool) -> [a] -> Bool
all3 p xs = foldr (&&) True (map p xs)

all4 :: (a -> Bool) -> [a] -> Bool
all4 p = foldr (&&) True . map p


any1 :: (a -> Bool) -> [a] -> Bool
any1 p = not . null . dropWhile (not . p)

any2 :: (a -> Bool) -> [a] -> Bool
any2 p xs = not (all (\ x -> not (p x)) xs)

any3 :: (a -> Bool) -> [a] -> Bool
any3 p xs = foldr (\ x acc -> (p x) || acc) False xs

any4 :: (a -> Bool) -> [a] -> Bool
any4 p xs = foldr (||) False (map p xs)


takeWhile1 _ [] = []
takeWhile1 p (x:xs)
  | p x = x : takeWhile1 p xs
  | otherwise = []

dropWhile1 _ [] = []
dropWhile1 p (x:xs)
  | p x = dropWhile1 p xs
  | otherwise = x : xs

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldl (\ xs x -> xs ++ [f x]) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\ x xs -> if p x then x : xs else xs) []

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldl (\ xs x -> if p x then xs ++ [x] else xs) []



dec2int :: [Integer] -> Integer
dec2int = foldl (\ a x -> a * 10 + x) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 f = \ x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \ (x, y) -> f x y


unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin2 = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop82 = unfold null (take 8) (drop 8)


map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

iterate1 f = unfold (const False) id f


f x = x > 3


