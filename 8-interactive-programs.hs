import System.IO

a :: IO (Char, Char)
a = do x <- getChar
       getChar
       y <- getChar
       return (x,y)

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

hangman :: IO ()
hangman =
  do putStrLn "Think of a word: "
     word <- sgetLine
     putStrLn "Try to guess it:"
     guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)


getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word =
  do putStr "> "
     xs <- getLine
     if xs == word then
       putStrLn "You got it!"
     else
       do putStrLn (diff word xs)
          guess word

diff :: String -> String -> String
diff xs ys =
  [if elem x ys then x else '-' | x <- xs ]



-- Homework

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs


putStrLn' :: String -> IO ()
putStrLn' xs = putStr' xs >> putChar '\n'


putStrLn1' :: String -> IO ()
putStrLn1' [] = putChar '\n'
putStrLn1' xs = putStr' xs >> putStr' "\n"


getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                do return []
              else
                do xs <- getLine'
                   return (x:xs)


getLine2 :: IO String
getLine2 = get []

get :: String -> IO String
get xs =
  do x <- getChar
     case x of
       '\n' -> return xs
       _    -> get (xs ++ [x])





interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)


sequence_' :: Monad m => [m a] -> m ()
--sequence_' [] = return ()
--sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

--sequence_' [] = return ()
--sequence_' (m:ms) = m >> sequence_' ms

--sequence_' [] = return ()
--sequence_' (m:ms) = m >>= \ _ -> sequence_' ms

sequence_' ms = foldr (>>) (return ()) ms


sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms)
  = m >>=
      \ a ->
        do as <- sequence' ms
           return (a:as)


--sequence' ms = foldr func (return []) ms
--  where
--    func :: (Monad m) => m a -> m [a] -> m [a]
--    func m acc
--      = do x <- m
--           xs <- acc
--           return (x:xs)


--sequence' [] = return []
--sequence' (m:ms)
--  = do a <- m
--       as <- sequence' ms
--       return (a:as)

--  mapM' (\ xs -> xs ++ ['a']) [getLine', getLine']
--mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM' f as = sequence' (map f as)



-- (a -> b -> m a)
-- (\a b -> putChar b >> return (b : a ++ [b]))


foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = a
foldLeftM f a (x:xs) = foldLeftM f (f a x (>>=) return ()) xs


--liftM :: Monad m => (a -> b) -> m a -> m b
--liftM f m
--  = do x <- m
--       return (f x)










