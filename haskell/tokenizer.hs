data Token = NUMBER Double
           | VAR Char
           | FUNCTION String
           | OP Char
           | EndOfInput Char
           deriving (Show, Eq)
    
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^()"

tokenize :: String -> [Token]
tokenize [] = [EndOfInput '$']
tokenize (c:cs)
    | isDigit c  = let (number, rest) = span (\x -> isDigit x || x == '.') (c:cs)
                   in NUMBER (read number) : tokenize rest
    | isAlpha c  = let (func, rest) = span isAlpha (c:cs)
                   in case func of
                        "X" -> VAR 'X' : tokenize rest
                        _   -> FUNCTION func : tokenize rest
    | isOperator c = OP c : tokenize cs
    | c == ' ' = tokenize cs  -- Skip whitespace
    | otherwise = error $ "Unexpected character: " ++ [c]

-- More functions as needed
main :: IO ()
main = do
    let tokens = tokenize "X^2 + sin(X) - 3.14"
    print tokens
