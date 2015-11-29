increment a = a+1

length1 :: [a] -> Int
length1 (x:xs) = foldr (\ _ n -> n+1) 0 (x:xs)


reverse1 :: [a] -> [a]
reverse1 (x:xs) = foldr (\ a xs -> xs ++ [a] ) [] (x:xs)


{-map1 :: (a -> a) -> [a] -> [a]-}
{-map1 f (x:xs) = foldr (\ x xs) [] (x:xs)-}

-- filter1 :: f -> [a] -> [a]
-- filter1 (x:xs) f = foldr (\ f ) [] (x:xs)
