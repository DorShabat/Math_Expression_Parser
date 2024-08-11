import qualified Data.Map as Map
import Data.Time.Clock

-- Tokenizer
-- Define the token types
data Token = NUMBER Double
           | VAR Char
           | FUNCTION String
           | OP Char
           | EndOfInput Char
           deriving (Show, Eq)

-- Helper function to check if a character is a digit
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Helper function to check if a character is a digit or a dot
isDigitOrDot :: Char -> Bool
isDigitOrDot x = isDigit x || x == '.'

-- Helper function to check if a character is an alphabet
isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- Helper function to check if a character is an operator
isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^()"


-- Tokenize the input string
-- span :: (a -> Bool) -> [a] -> ([a], [a]), span p xs returns the longest prefix of xs of elements that satisfy p
tokenize :: String -> [Token]
tokenize [] = [EndOfInput '$']
tokenize (c:cs)
    | isDigit c  = let (number, rest) = span isDigitOrDot (c:cs) 
                   in NUMBER (read number) : tokenize rest
    | isAlpha c  = let (func, rest) = span isAlpha (c:cs)
                   in case func of
                        "X" -> VAR 'X' : tokenize rest
                        _   -> FUNCTION func : tokenize rest
    | isOperator c = OP c : tokenize cs
    | c == ' ' = tokenize cs  -- Skip whitespace
    | otherwise = error $ "Unexpected character: " ++ [c]

----------------------------------------------------------------------------
-- (, ), *, +, -, /, NUMBER, X, ^, arccos, arcsin, arctg, cos, exp, ln, sin, tg, $, E, T, F, G, Func
stringToIndex :: String -> Int
stringToIndex "(" = 0
stringToIndex ")" = 1
stringToIndex "*" = 2
stringToIndex "+" = 3
stringToIndex "-" = 4
stringToIndex "/" = 5
stringToIndex "NUMBER" = 6
stringToIndex "X" = 7
stringToIndex "^" = 8
stringToIndex "arccos" = 9
stringToIndex "arcsin" = 10
stringToIndex "arctg" = 11
stringToIndex "cos" = 12
stringToIndex "exp" = 13
stringToIndex "ln" = 14
stringToIndex "sin" = 15
stringToIndex "tg" = 16
stringToIndex "$" = 17
stringToIndex "E" = 18
stringToIndex "T" = 19
stringToIndex "F" = 20
stringToIndex "G" = 21
stringToIndex "Func" = 22
stringToIndex _ = error "Unknown symbol"



-- parsing table
-- Define the types
type State = String
type Symbol = String
type Action = String
type ParsingTable = [[String]]-- defines ParsingTable as a nested map structure

-- Define the parsing table
getParsingTable :: ParsingTable
getParsingTable = [
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "1", "2", "3", "4", "7", "0", "s5"],
        ["0", "0", "0", "s18", "s19", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "acc", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( E -> T )", "s20", "r( E -> T )", "r( E -> T )", "s21", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( E -> T )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( T -> F )", "r( T -> F )", "r( T -> F )", "r( T -> F )", "r( T -> F )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( T -> F )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( F -> G )", "r( F -> G )", "r( F -> G )", "r( F -> G )", "r( F -> G )", "0", "0", "s22", "0", "0", "0", "0", "0", "0", "0", "0", "r( F -> G )", "0", "0", "0", "0", "0", "0", "0"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "23", "2", "3", "4", "7", "0", "s5"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "0", "0", "0", "24", "7", "0", "s5"],
        ["s25", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "s25"],
        ["0", "r( G -> NUMBER )", "r( G -> NUMBER )", "r( G -> NUMBER )", "r( G -> NUMBER )", "r( G -> NUMBER )", "0", "0", "r( G -> NUMBER )", "0", "0", "0", "0", "0", "0", "0", "0", "r( G -> NUMBER )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( G -> X )", "r( G -> X )", "r( G -> X )", "r( G -> X )", "r( G -> X )", "0", "0", "r( G -> X )", "0", "0", "0", "0", "0", "0", "0", "0", "r( G -> X )", "0", "0", "0", "0", "0", "0", "0"],
        ["r( Func -> sin )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> sin )"],
        ["r( Func -> cos )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> cos )"],
        ["r( Func -> tg )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> tg )"],
        ["r( Func -> arcsin )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> arcsin )"],
        ["r( Func -> arccos )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> arccos )"],
        ["r( Func -> arctg )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> arctg )"],
        ["r( Func -> exp )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> exp )"],
        ["r( Func -> ln )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( Func -> ln )"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "0", "26", "3", "4", "7", "0", "s5"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "0", "27", "3", "4", "7", "0", "s5"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "0", "0", "28", "4", "7", "0", "s5"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "0", "0", "29", "4", "7", "0", "s5"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "0", "0", "30", "4", "7", "0", "s5"],
        ["0", "s31", "0", "s18", "s19", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( G -> - G )", "r( G -> - G )", "r( G -> - G )", "r( G -> - G )", "r( G -> - G )", "0", "0", "r( G -> - G )", "0", "0", "0", "0", "0", "0", "0", "0", "r( G -> - G )", "0", "0", "0", "0", "0", "0", "0"],
        ["s5", "0", "0", "0", "s6", "0", "s8", "s9", "0", "s14", "s13", "s15", "s11", "s16", "s17", "s10", "s12", "0", "32", "2", "3", "4", "7", "0", "s5"],
        ["0", "r( E -> E + T )", "s20", "r( E -> E + T )", "r( E -> E + T )", "s21", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( E -> E + T )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( E -> E - T )", "s20", "r( E -> E - T )", "r( E -> E - T )", "s21", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( E -> E - T )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( T -> T * F )", "r( T -> T * F )", "r( T -> T * F )", "r( T -> T * F )", "r( T -> T * F )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( T -> T * F )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( T -> T / F )", "r( T -> T / F )", "r( T -> T / F )", "r( T -> T / F )", "r( T -> T / F )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( T -> T / F )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( F -> G ^ F )", "r( F -> G ^ F )", "r( F -> G ^ F )", "r( F -> G ^ F )", "r( F -> G ^ F )", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "r( F -> G ^ F )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( G -> ( E ) )", "r( G -> ( E ) )", "r( G -> ( E ) )", "r( G -> ( E ) )", "r( G -> ( E ) )", "0", "0", "r( G -> ( E ) )", "0", "0", "0", "0", "0", "0", "0", "0", "r( G -> ( E ) )", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "s33", "0", "s18", "s19", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"],
        ["0", "r( G -> Func ( E ) )", "r( G -> Func ( E ) )", "r( G -> Func ( E ) )", "r( G -> Func ( E ) )", "r( G -> Func ( E ) )", "0", "0", "r( G -> Func ( E ) )", "0", "0", "0", "0", "0", "0", "0", "0", "r( G -> Func ( E ) )", "0", "0", "0", "0", "0", "0", "0"]
        ]




-- Helper function to look up: ParsingTable[State][Symbol]
-- State can be 0 - 33
-- Symbol can be: (, ), *, +, -, /, NUMBER, X, ^, arccos, arcsin, arctg, cos, exp, ln, sin, tg, $, E, T, F, G, Func
-- when action table is: (, ), *, +, -, /, NUMBER, X, ^, arccos, arcsin, arctg, cos, exp, ln, sin, tg, $ and the goto table is: E, T, F, G, Func
lookupAction :: State -> Symbol -> ParsingTable -> Maybe Action
lookupAction state symbol table =  -- ParsingTable[State][Symbol]
    let row = read state :: Int
        col = stringToIndex symbol
    in if row < length table then
        Just (table !! row !! col)
    else
        Nothing
---------------------------------------------------------------------------

data LRParser = LRParser
    { parsingTable :: ParsingTable
    , xValue   :: Double  -- Value of X
    }

-- Define Mode type: EVAL for evaluating the expression, DERIVATIVE for differentiating the expression
data Mode = EVAL | DERIVATIVE deriving (Eq)

-- Define the Value type to store the result of the parsing
data Value = ValString String
           | ValDouble Double
           | ValNode Node
           deriving (Show, Eq)

-- Define the Node type to store the operator tree
data Node = Node
    { value    :: String
    , left     :: Maybe Node
    , right    :: Maybe Node
    }
    deriving (Show, Eq)

-- Function to parse the input tokens using the LR parser
parse :: LRParser -> [Token] -> Mode -> Either String Value
parse parser tokens mode = parse' [0] tokens []
    where
        parse' :: [Int] -> [Token] -> [Value] -> Either String Value
        parse' (state:states) (token:tokens) values =
            case lookupAction (show state) (tableColNames token) (parsingTable parser) of
                Just action ->
                    if take 1 action == "s" then
                        -- Shift operation
                        let nextState = read (drop 1 action) :: Int -- sNUM -> NUM . 
                            newValues = if mode == EVAL  -- EVAL mode
                                then case token of
                                    NUMBER n    -> ValDouble n : values
                                    VAR 'X'     -> ValDouble (xValue parser) : values
                                    FUNCTION f  -> ValString f : values
                                    OP op       -> ValString [op] : values
                                    _           -> error "Unexpected token type"
                                else case token of      -- DERIVATIVE mode
                                    NUMBER n    -> ValNode (Node (show n) Nothing Nothing) : values
                                    VAR 'X'     -> ValNode (Node "X" Nothing Nothing) : values
                                    FUNCTION f  -> ValString f : values
                                    OP op       -> ValString [op] : values
                                    _           -> error "Unexpected token type"
                        in parse' (nextState : state : states) tokens newValues
                    else if take 1 action == "r" then
                        -- Reduce operation
                        let (lhs, rhs) = parseProduction action
                            valuesToReduce = take (length rhs) values
                            remainingValues = drop (length rhs) values
                            remainingStats = drop (length rhs) (state : states)
                            result = if mode == EVAL
                                then evaluate parser lhs (reverse valuesToReduce)   -- EVAL mode
                                else build_tree parser lhs (reverse valuesToReduce) -- DERIVATIVE mode
                            -- Look up the new state after the reduction
                            newState = case lookupAction (show (head remainingStats)) lhs (parsingTable parser) of -- "from go to table"
                                Just nextAction -> read (nextAction) :: Int
                                Nothing         -> error "Failed to find next state during reduction"
                            newValues = result : remainingValues
                            -- Add newState to states stack
                            updatedStates = newState : remainingStats
                        in parse' updatedStates (token:tokens) newValues
                    else if action == "acc" then
                        if mode == EVAL 
                            then Right (head values)  -- EVAL mode
                            else case head values of  -- DERIVATIVE mode
                                ValNode node -> Right (ValString (treeToString (differentiate node)))
                                _ -> error "Expected a node in DERIVATIVE mode"
                    else
                        Left "Unknown action"
                Nothing -> Left "Parsing error"          
        parse' _ [] _ = Left "Incomplete input"
                        
        -- Helper function to get the column name for a token
        tableColNames :: Token -> String
        tableColNames (NUMBER _) = "NUMBER"
        tableColNames (VAR _) = "X"
        tableColNames (FUNCTION func) = func
        tableColNames (OP op) = [op]
        tableColNames (EndOfInput _) = "$"

        -- Helper function to parse a production rule, for example "r( E -> T )" -> ("E", ["T"])
        parseProduction :: String -> (String, [String])
        parseProduction prod = 
            let (lhs, rhs) = break (== '-') (drop 2 (init prod)) -- init prod removes the last ')' , drop 2 removes the 'r('. 
                trimmedLhs = trim lhs   -- Remove leading/trailing spaces from lhs
                trimmedRhs = words (drop 2 rhs) -- `words` already handles trimming spaces between symbols in rhs
            in (trimmedLhs, trimmedRhs)

        -- Helper function to trim spaces
        trim :: String -> String
        trim str = unwords (words str)-- words splits the string into words (removing any extra spaces), unwords joins the words back into a string

-- Function to evaluate the parsed expression
evaluate :: LRParser -> String -> [Value] -> Value
evaluate _ "E" [v] = v
evaluate _ "E" [ValDouble v1, ValString "+", ValDouble v2] = ValDouble (v1 + v2)
evaluate _ "E" [ValDouble v1, ValString "-", ValDouble v2] = ValDouble (v1 - v2)

evaluate _ "T" [v] = v
evaluate _ "T" [ValDouble v1, ValString "*", ValDouble v2] = ValDouble (v1 * v2)
evaluate _ "T" [ValDouble v1, ValString "/", ValDouble v2] = ValDouble (v1 / v2)

evaluate _ "F" [v] = v
evaluate _ "F" [ValDouble v1, ValString "^", ValDouble v2] = ValDouble (v1 ** v2)

evaluate _ "G" [v] = v
evaluate _ "G" [ValString "-", ValDouble v] = ValDouble (-v)
evaluate _ "G" [ValString "(", v, _] = v
evaluate _ "G" [ValString func, ValString "(", ValDouble arg, ValString ")"] = ValDouble (applyFunction func arg)

-- If none of the patterns match, return the first value
evaluate _ _ (v:_) = v
evaluate _ _ _ = error "Unknown evaluation"

-- Helper function to apply a function to an argument
applyFunction :: String -> Double -> Double
applyFunction "sin" arg = sin(arg)
applyFunction "cos" arg = cos(arg)
applyFunction "tg" arg  = tan(arg)
applyFunction "ln" arg  = log(arg)
applyFunction "exp" arg = exp(arg)
applyFunction "arcsin" arg = asin(arg)
applyFunction "arccos" arg = acos(arg)
applyFunction "arctg"  arg = atan(arg)
applyFunction funcName _ = error $ "Unknown function: " ++ funcName

-- Function to build a operator tree from the parsed expression
build_tree :: LRParser -> String -> [Value] -> Value
build_tree _ "E" [v] = v
build_tree _ "E" [ValNode n1, ValString "+", ValNode n2] = ValNode (Node "+" (Just n1) (Just n2))
build_tree _ "E" [ValNode n1, ValString "-", ValNode n2] = ValNode (Node "-" (Just n1) (Just n2))

build_tree _ "T" [v] = v
build_tree _ "T" [ValNode n1, ValString "*", ValNode n2] = ValNode (Node "*" (Just n1) (Just n2))
build_tree _ "T" [ValNode n1, ValString "/", ValNode n2] = ValNode (Node "/" (Just n1) (Just n2))

build_tree _ "F" [v] = v
build_tree _ "F" [ValNode n1, ValString "^", ValNode n2] = ValNode (Node "^" (Just n1) (Just n2))

build_tree _ "G" [v] = v
build_tree _ "G" [ValString "-", ValNode n] = ValNode (Node "-" Nothing (Just n))
build_tree _ "G" [ValString "(", v, ValString ")"] = v
build_tree _ "G" [ValString func, ValString "(", ValNode arg, ValString ")"] = ValNode (Node func Nothing (Just arg))

build_tree _ _ (v:_) = v
build_tree _ _ _ = error "Unknown tree construction"

-- Function to differentiate the operator tree, returning a new derived tree
differentiate :: Node -> Node
differentiate (Node "X" _ _) = Node "1" Nothing Nothing  -- Derivative of X is 1
differentiate (Node val _ _) 
    | isNumeric val = Node "0" Nothing Nothing  -- Derivative of a constant is 0
differentiate (Node "+" (Just l) (Just r)) = Node "+" (Just (differentiate l)) (Just (differentiate r))
differentiate (Node "-" Nothing (Just r)) = Node "-" Nothing (Just (differentiate r))
differentiate (Node "-" (Just l) (Just r)) = Node "-" (Just (differentiate l)) (Just (differentiate r))
differentiate (Node "*" (Just l) (Just r)) =
    Node "+" (Just (Node "*" (Just (differentiate l)) (Just r)))
             (Just (Node "*" (Just l) (Just (differentiate r))))
differentiate (Node "/" (Just l) (Just r)) =
    Node "/" (Just (Node "-" (Just (Node "*" (Just (differentiate l)) (Just r)))
                           (Just (Node "*" (Just l) (Just (differentiate r))))))
             (Just (Node "^" (Just r) (Just (Node "2" Nothing Nothing))))
differentiate (Node "^" (Just base) (Just exponent)) =
    Node "*" (Just (Node "^" (Just base) (Just exponent)))
             (Just (Node "+"
                   (Just (Node "*" (Just exponent)
                                  (Just (Node "/" (Just (differentiate base)) (Just base)))))
                   (Just (Node "*" (Just (Node "ln" Nothing (Just base)))
                                  (Just (differentiate exponent))))))
differentiate (Node "sin" Nothing (Just arg)) =
    Node "*" (Just (Node "cos" Nothing (Just arg))) (Just (differentiate arg))
differentiate (Node "cos" Nothing (Just arg)) =
    Node "*" (Just (Node "*" (Just (Node "-1" Nothing Nothing)) (Just (Node "sin" Nothing (Just arg))))) (Just (differentiate arg))
differentiate (Node "tg" Nothing (Just arg)) =
    Node "*" (Just (Node "/" (Just (Node "1" Nothing Nothing)) (Just (Node "^" (Just (Node "cos" Nothing (Just arg))) (Just (Node "2" Nothing Nothing)))))) (Just (differentiate arg))
differentiate (Node "arcsin" Nothing (Just arg)) =
    Node "*" 
        (Just (Node "/" (Just (Node "1" Nothing Nothing))
                            (Just (Node "^" (Just (Node "-" (Just (Node "1" Nothing Nothing)) (Just (Node "^" (Just arg) (Just (Node "2" Nothing Nothing)))))) (Just (Node "0.5" Nothing Nothing)))))) (Just (differentiate arg))
differentiate (Node "arccos" Nothing (Just right)) =
    Node "*" 
        (Just (Node "-1" Nothing Nothing)) 
        (Just (Node "*" 
                (Just (Node "/" 
                        (Just (Node "1" Nothing Nothing)) 
                        (Just (Node "^" 
                                (Just (Node "-" 
                                        (Just (Node "1" Nothing Nothing)) 
                                        (Just (Node "^" (Just right) (Just (Node "2" Nothing Nothing))))))
                                (Just (Node "0.5" Nothing Nothing))))))
                (Just (differentiate right))))

differentiate (Node "arctg" Nothing (Just right)) =
    Node "*" 
        (Just (Node "/" 
                (Just (Node "1" Nothing Nothing)) 
                (Just (Node "+" 
                        (Just (Node "1" Nothing Nothing)) 
                        (Just (Node "^" (Just right) (Just (Node "2" Nothing Nothing))))))))
        (Just (differentiate right))
differentiate (Node "exp" Nothing (Just arg)) =
    Node "*" (Just (Node "exp" Nothing (Just arg))) (Just (differentiate arg))
differentiate (Node "ln" Nothing (Just arg)) =
    Node "*" (Just (Node "/" (Just (Node "1" Nothing Nothing)) (Just arg))) (Just (differentiate arg))
differentiate node = error $ "Unsupported operation: " ++ value node

-- Helper function to check if a string is numeric
isNumeric :: String -> Bool
isNumeric = all (`elem` "0123456789.")

-- Function to convert a Node tree to a string
treeToString :: Node -> String
treeToString (Node val Nothing Nothing) = val
treeToString (Node val left right) =
    let leftStr  = maybe "" treeToString left
        rightStr = maybe "" treeToString right
    in if val `elem` ["cos", "sin", "ln", "tg", "arcsin", "arccos", "arctg", "exp"]
       then leftStr ++ " " ++ val ++ " (" ++ rightStr ++ ")"
       else "(" ++ leftStr ++ " " ++ val ++ " " ++ rightStr ++ ")"


main :: IO ()
main = do
    tt_start_tokenize <- getCurrentTime
    let x_value = 2.0
    let expression = "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654" ++
                    "+2*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)"++
                    "+20*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)"++
                    "-(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)/10"++
                    "+(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)^2"++
                    "+2*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)"++
                    "*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)"


--X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654+2*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)+20*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)-(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)/10+(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)^2+2*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)
    
    putStrLn ("f(X) = " ++ expression)
    putStrLn ("len of expression f(X): " ++ show (length expression))

    let parser = LRParser getParsingTable x_value 
    let tokens = tokenize expression  -- Tokenize the expression
    
    tt_end_tokenize <- getCurrentTime
    let lrParserTokenizeRunTime = diffUTCTime tt_end_tokenize tt_start_tokenize
    putStrLn ("\nTokenizer time: " ++ show lrParserTokenizeRunTime)

    -- Evaluate f(X) , X = x_value
    tt_start_eval <- getCurrentTime
    case parse parser tokens EVAL of -- Evaluate the expression
        Right (ValDouble result) -> do
            tt_end_eval <- getCurrentTime
            let evalRunTime = diffUTCTime tt_end_eval tt_start_eval
            putStrLn $ "\nEval(f(" ++ show x_value ++ ")) = " ++ show result
            putStrLn $ "Evaluation time: " ++ show evalRunTime
        Left errorMsg -> do
            tt_end_eval <- getCurrentTime
            let evalRunTime = diffUTCTime tt_end_eval tt_start_eval
            putStrLn $ "Error: " ++ errorMsg
            putStrLn $ "Evaluation time: " ++ show evalRunTime

    -- Differentiate f(X) and evaluate the derivative at X = x_value
    tt_start_derivative <- getCurrentTime
    case parse parser tokens DERIVATIVE of -- Differentiate the expression
        Right (ValString derivative) -> do
            tt_end_derivative <- getCurrentTime
            let derivativeRunTime = diffUTCTime tt_end_derivative tt_start_derivative
            putStrLn $ "\nDiff(f(X)) = " ++ derivative
            putStrLn $ "Differentiate time: " ++ show derivativeRunTime
            -- Now parse the derivative result in EVAL mode
            let derivativeTokens = tokenize derivative
            case parse parser derivativeTokens EVAL of -- Evaluate the derivative at X = x_value
                Right (ValDouble evalDerivative) -> putStrLn $ "\nDiff(f(" ++ show x_value ++ ")) = " ++ show evalDerivative
                Left errorMsg -> putStrLn $ "Error during evaluation of derivative: " ++ errorMsg
        Right _ -> putStrLn "DERIVATIVE did not produce a string."
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
