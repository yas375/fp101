import Prelude hiding (return)

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \ inp -> [(v, inp)]

failure :: Parser a
failure = \ inp -> []

item :: Parser Char
item = \ inp -> case inp of
                  [] -> []
                  (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

{-
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \ inp -> case parse p inp of
                     [] -> []
                     [(v, out)] -> parse (f v) out
-}

{-
p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x, y)
-}

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case parse p inp of
                     [] -> parse q inp
                     [(v, out)] -> [(v, out)]

{-
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure
-}


